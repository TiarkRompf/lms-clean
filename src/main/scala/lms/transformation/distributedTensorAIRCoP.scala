package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}
import lms.transformation.util.DataStructure

import Backend._


abstract class DistributeTensorAIRCoP extends Transformer {

  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeDistributedTensorTypeLess._
  // import CUDATypeLess._
  // import CUBLASTypeLess._

  val forwardNodes = mutable.ArrayBuffer[Node]()
  val weightNodes = mutable.ArrayBuffer[Node]()
  val backwardNodes = mutable.ArrayBuffer[()=>Unit]()

  // this is the gradient map from OLD value tensors to NEW gradient tensors
  val grad_map = mutable.HashMap[Backend.Sym, TENSOR]()
  val momentum_map = mutable.HashMap[Backend.Sym, TENSOR]()

  def traverseModule(ns: Seq[Node], res: Block): Backend.Exp = {
    // Step 1: Collection Phase
    ns.foreach {
      case n@Node(s, "tensor_input", _, _) => forwardNodes += n
      case n@Node(s, "tensor_weight", _, _) => weightNodes += n
      case n@Node(s, "tensor_mult", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::(b:Backend.Sym)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        // save forward op in forwardNodes
        forwardNodes += n
        // save backward op in backwardNodes
        (() => {
          val a_tensor = new TENSOR(transform(a))
          val b_grad = a_tensor * (grad_map(s), anno)
          grad_map(b) += (b_grad, anno); ()
        }) +=: backwardNodes
        (() => {
          val b_tensor = new TENSOR(transform(b))
          val a_grad = b_tensor * (grad_map(s), anno)
          grad_map(a) += (a_grad, anno); ()
        }) +=: backwardNodes
      case n@Node(s, "tensor_dot", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::(b:Backend.Sym)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        // save forward op in forwardNodes
        forwardNodes += n
        // save backward op in backwardNodes
        (() => {
          val b_tensor = new TENSOR(transform(b))
          val b_transpose = b_tensor.transpose(anno)
          val a_grad = grad_map(s) dot (b_transpose, anno)
          grad_map(a) += (a_grad, anno); ()
        }) +=: backwardNodes
        (() => {
          val a_tensor = new TENSOR(transform(a))
          val a_transpose = a_tensor.transpose(anno)
          val b_grad = a_transpose dot (grad_map(s), anno)
          grad_map(b) += (b_grad, anno); ()
        }) +=: backwardNodes
      case n => throw new Exception(s"$n todo")
    }
    // result of the block
    implicit val pos = Adapter.oldSourceMap(ns.last.n)
    (() => {
      val result = new TENSOR(transform(res.res))
      val grad = ONES(result.tensor_type, result.annotation)
      grad_map(res.res.asInstanceOf[Backend.Sym]) = grad
    }) +=: backwardNodes
    // collect all weight syms and all forward syms
    val weightSyms = weightNodes.map(s => s.n)
    val forwardSyms = forwardNodes.map(s => s.n)


    // Step 2: Generation Phase
    traverseWeights(weightNodes) {
      for (i <- (0 until 5): Rep[Range]) {
        // FIXME(feiw) use 5 training iteration for now
        traverseForward(forwardNodes) {
          traverseBackward(backwardNodes, forwardSyms) {
            traverseOptimization(weightSyms) { () => () }
          }
        }
      }
    }
    // FIXME(feiw) change res to updated weights
    val updated_weights = weightSyms.map(w => subst(w)).toList
    updated_weights.foreach(w => (new TENSOR(w)).save)
    Backend.Const(())
  }

  def traverseWeights(weights: mutable.ArrayBuffer[Node])(cont: => Unit) = {
    for (w <- weights) {
      traverse(w)
    }
    cont
  }

  def traverseForward(forwards: mutable.ArrayBuffer[Node])(cont: => Unit) = {
    for (f <- forwards) {
      traverse(f)
    }
    cont
  }

  def traverseBackward(backwards: mutable.ArrayBuffer[()=>Unit], forwardSyms: mutable.ArrayBuffer[Backend.Sym])(cont: => Unit) = {
    for (fs <- forwardSyms) {
      implicit val pos = Adapter.oldSourceMap(fs)
      val node = new TENSOR(transform(fs))
      val grad = ZEROS(node.tensor_type, node.annotation)
      grad_map(fs.asInstanceOf[Backend.Sym]) = grad
    }
    for (b <- backwards) {
      b()
    }
    cont
  }

  def traverseOptimization(weightSyms: mutable.ArrayBuffer[Backend.Sym])(cont: => Unit) = {
    for (w <- weightSyms) {
      implicit val pos = Adapter.oldSourceMap(w)
      val annotation = (new TENSOR(w, useOldMetadata=true)).annotation
      (new TENSOR(transform(w))).optimize(grad_map(w), momentum_map(w), annotation)
    }
    cont
  }

  def transfromModuleBlock(b: Block): Block = b match {
    case b @ Block(Nil, res, block, eff) => g.reify {
      scheduleBlock(b)(traverseModule)
    }
  }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "module", (b @ Block(in, y, ein, eff))::_, _) =>
      g.reflectWrite("module", transfromModuleBlock(b))(Adapter.CTRL)

    case Node(s, "tensor_weight", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val new_weight = WEIGHT(tt, anno)
      grad_map(s) = ZEROS(tt, anno)
      momentum_map(s) = ZEROS(tt, anno)
      // FIXME(feiw) how do we deal with the case where multiple nodes are generated?
      new_weight.x

    case Node(s, "tensor_input", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val new_input = INPUT(tt, anno)
      new_input.x

    case Node(s, "tensor_mult", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val left = new TENSOR(transform(x))
      val right = new TENSOR(transform(y))
      val res = left * (right, anno)
      res.x

    case _ => super.transform(n)
  }

  override def transform(graph: Graph): Graph = {
    assert (g == null)
    g = new GraphBuilderOpt()
    Adapter.g = g
    try {
      super.transform(graph)
    } finally {
      g = null; Adapter.g = null
    }
  }
}
