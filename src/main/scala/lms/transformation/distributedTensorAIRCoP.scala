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


abstract class DistributeTensorAIRCoP extends Transformer with DataStructure {
  override val name = "DistributeTensorAIRCoP"

  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  val forwardNodes = mutable.ArrayBuffer[Node]()
  val weightNodes = mutable.ArrayBuffer[Node]()
  val backwardNodes = mutable.ArrayBuffer[()=>Unit]()

  // this is the gradient map from OLD value tensors to NEW gradient tensors
  val momentumMap = mutable.HashMap[Backend.Sym, TENSOR]()
  val gradMap_ = mutable.HashMap[Backend.Sym, Backend.Sym]()
  val gradMap = GradMapWrapper(gradMap_)

  def traverseModule(iter: Int)(ns: Seq[Node], res: Block): Backend.Exp = {
    // Step 1: Collection Phase
    ns.foreach { n => aircopCollect(n, forwardNodes, weightNodes, backwardNodes, gradMap, momentumMap, transform) }
    // result of the block
    implicit val pos = Adapter.oldSourceMap(ns.last.n)
    (() => {
      val result = new TENSOR(transform(res.res))
      // FIXME(feiw) this should not be check, but be log.
      result.check("loss")
      val grad = ONES(result.resultType, result.annotation)
      gradMap(res.res) = grad
    }) +=: backwardNodes
    // collect all weight syms and all forward syms
    val weightSyms = weightNodes.map(s => s.n)
    val forwardSyms = forwardNodes.map(s => s.n)


    // Step 2: Generation Phase
    traverseWeights(weightNodes) {
      for (i <- (0 until iter): Rep[Range]) {
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

  def traverseModule(loss: String)(ns: Seq[Node], res: Block): Backend.Exp = {
    // Step 1: Collection Phase
    ns.foreach { n => aircopCollect(n, forwardNodes, weightNodes, backwardNodes, gradMap, momentumMap, transform) }
    // result of the block
    implicit val pos = Adapter.oldSourceMap(ns.last.n)
    (() => {
      val result = new TENSOR(transform(res.res))
      result.check(loss)
      val grad = ONES(result.resultType, result.annotation)
      gradMap(res.res) = grad
    }) +=: backwardNodes
    // collect all weight syms and all forward syms
    val weightSyms = weightNodes.map(s => s.n)
    val forwardSyms = forwardNodes.map(s => s.n)
    val inputSyms = forwardNodes.filter {
      case Node(s, "tensor_input", _, _) => true
      case _ => false
    }.map(s => s.n)

    // Step 2: Generation Phase
    traverseWeights(weightNodes) { // FIXME(feiw) maybe remove optimizer?
      traverseForward(forwardNodes) {
        traverseBackward(backwardNodes, forwardSyms) {
          checkGradients(weightSyms ++ inputSyms)
        }
      }
    }
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

      if (TENSOR.isTensor(transform(fs))) {
        val node = new TENSOR(transform(fs))
        val grad = ZEROS(node.resultType.map(_+"_grad"), node.annotation)
        gradMap(fs) = grad
      } else if (TENSORS.isTensors(transform(fs))) {
        val oldOp = new TENSORS(fs, useOldMetadata = true)
        val grads = oldOp.resultTypes.map(t=>ZEROS(t.map(_+"_grad"), oldOp.annotation))
        gradMap(fs) = Adapter.g.reflect("tuple-view", grads.map(_.x): _*)
      }
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
      Optimize(new TENSOR(transform(w)), gradMap(w), momentumMap(w), annotation)
    }
    cont
  }

  def checkGradients(weightSyms: => mutable.ArrayBuffer[Backend.Sym]) = {
    for (w <- weightSyms) {
      val weight = new TENSOR(transform(w))
      implicit val __pos = weight.pos
      weight.resultType.tensorName match {
        case Some(name) => gradMap(w).check(name + "_grad") // FIXME(feiw) unfortunately hard-coded
        case None => throw new Exception("Missing checked file names for tensor")
      }
    }
  }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "module", (b @ Block(in, y, ein, eff))::_, _) => Backend.Const(())

    case Node(s, "@", List(a: Backend.Sym, Backend.Const(iter: Int)), _) =>
      // the `iter: Int` means that it is the training call with `iter` iterations
      Adapter.oldDefsCache(a) match {
        case Node(s, "module", (b @ Block(in, y, ein, eff))::_, _) => scheduleBlock(b)(traverseModule(iter))
        case _ => super.transform(n)
      }

    case Node(s, "@", List(a: Backend.Sym, Backend.Const(loss: String)), _) =>
      // the `loss: String` means that it is the test call with `loss` being the filename of the expect loss values
      Adapter.oldDefsCache(a) match {
        case Node(s, "module", (b @ Block(in, y, ein, eff))::_, _) => scheduleBlock(b)(traverseModule(loss))
        case _ => super.transform(n)
      }

    case Node(s, "tensor_weight", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val new_weight = WEIGHT(tt, anno)
      gradMap(s) = ZEROS(tt.map(_+"_grad"), anno)
      momentumMap(s) = ZEROS(tt.map(_+"_momentum"), anno)
      new_weight.x

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
