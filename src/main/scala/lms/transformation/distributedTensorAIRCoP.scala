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
  val backwardNodes = mutable.ArrayBuffer[()=>TENSOR]()

  // this is the gradient map from OLD value tensors to NEW gradient tensors
  val grad_map = mutable.HashMap[Backend.Sym, Backend.Sym]()
  val momentum_map = mutable.HashMap[Backend.Sym, Backend.Sym]()

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
          val b_grad = a_tensor * (new TENSOR(grad_map(s)), anno)
          // FIXME(feiw) handle fanout > 1
          new TENSOR(grad_map(b)) += b_grad
          b_grad
          // grad_map(b) = b_grad.x.asInstanceOf[Backend.Sym]
          // Adapter.g.globalDefsCache(b_grad.x.asInstanceOf[Backend.Sym])
        }) +=: backwardNodes
        (() => {
          val b_tensor = new TENSOR(transform(b))
          val a_grad = b_tensor * (new TENSOR(grad_map(s)), anno)
          new TENSOR(grad_map(a)) += a_grad
          a_grad
          // grad_map(a) = a_grad.x.asInstanceOf[Backend.Sym]
          // Adapter.g.globalDefsCache(a_grad.x.asInstanceOf[Backend.Sym])
        }) +=: backwardNodes
      case n@Node(s, "tensor_dot", tt::anno::(a:Backend.Sym)::(b:Backend.Sym)::_, _) =>
        // save forward op in forwardNodes
        forwardNodes += n
        // save backward op in backwardNodes
        (() => {
          ???
        }) +=: backwardNodes
      case n => throw new Exception(s"$n todo")
    }
    // result of the block
    implicit val pos = Adapter.oldSourceMap(ns.last.n)
    (() => {
      val result = new TENSOR(transform(res.res))
      val grad = ONES(result.tensor_type, result.annotation)
      grad_map(res.res.asInstanceOf[Backend.Sym]) = grad.x.asInstanceOf[Backend.Sym]
      grad
      // Adapter.g.globalDefsCache(grad.x.asInstanceOf[Backend.Sym])
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
    g.reflectEffect("save", updated_weights: _*)(updated_weights: _*)(Adapter.CTRL)
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

  def traverseBackward(backwards: mutable.ArrayBuffer[()=>TENSOR], forwardSyms: mutable.ArrayBuffer[Backend.Sym])(cont: => Unit) = {
    for (fs <- forwardSyms) {
      implicit val pos = Adapter.oldSourceMap(fs)
      val node = new TENSOR(transform(fs))
      val grad = ZEROS(node.tensor_type, node.annotation)
      grad_map(fs.asInstanceOf[Backend.Sym]) = grad.x.asInstanceOf[Backend.Sym]
    }
    for (b <- backwards) {
      b()
    }
    cont
  }

  def traverseOptimization(weightSyms: mutable.ArrayBuffer[Backend.Sym])(cont: => Unit) = {
    for (w <- weightSyms) {
      implicit val pos = Adapter.oldSourceMap(w)
      (new TENSOR(transform(w))).optimize(new TENSOR(grad_map(w)), new TENSOR(momentum_map(w)))
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
      val new_grad = WEIGHT(tt, anno)
      val new_momentum = WEIGHT(tt, anno)
      grad_map(s) = new_grad.x.asInstanceOf[Backend.Sym]
      momentum_map(s) = new_momentum.x.asInstanceOf[Backend.Sym]
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
