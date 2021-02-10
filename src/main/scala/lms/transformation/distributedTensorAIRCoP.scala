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
  override val name = "DistributeTensorAIRCoP"

  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  val forwardNodes = mutable.ArrayBuffer[Node]()
  val weightNodes = mutable.ArrayBuffer[Node]()
  val backwardNodes = mutable.ArrayBuffer[()=>Unit]()

  // this is the gradient map from OLD value tensors to NEW gradient tensors
  val gradMap = mutable.HashMap[Backend.Sym, TENSOR]()
  val momentumMap = mutable.HashMap[Backend.Sym, TENSOR]()

  def traverseModule(iter: Int)(ns: Seq[Node], res: Block): Backend.Exp = {
    // Step 1: Collection Phase
    ns.foreach { n => aircopCollect(n, forwardNodes, weightNodes, backwardNodes, gradMap, momentumMap, transform) }
    // result of the block
    implicit val pos = Adapter.oldSourceMap(ns.last.n)
    (() => {
      val result = new TENSOR(transform(res.res))
      val grad = ONES(result.tensor_type, result.annotation)
      gradMap(res.res.asInstanceOf[Backend.Sym]) = grad
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
        val grad = ZEROS(node.tensor_type, node.annotation)
        gradMap(fs.asInstanceOf[Backend.Sym]) = grad

      } else { // must be OPERATION
        val oldOp = new OPERATION(fs, useOldMetadata = true)
        val newOp = new OPERATION(transform(fs)).withSource(pos)
        for ((oldTensor, newTensor) <- oldOp.getResults zip newOp.getResults) {
          val grad = ZEROS(oldTensor.tensor_type, oldTensor.annotation)
          gradMap(oldTensor.x.asInstanceOf[Backend.Sym]) = grad
        }
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

  // def transformModuleBlock(b: Block): Block = b match {
  //   case b @ Block(Nil, res, block, eff) => g.reify {
  //     scheduleBlock(b)(traverseModule)
  //   }
  // }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "module", (b @ Block(in, y, ein, eff))::_, _) => Backend.Const(())

    case Node(s, "@", List(a: Backend.Sym, Backend.Const(iter: Int)), _) =>
      Adapter.oldDefsCache(a) match {
        case Node(s, "module", (b @ Block(in, y, ein, eff))::_, _) => scheduleBlock(b)(traverseModule(iter))
        case _ => super.transform(n)
      }

    case Node(s, "tensor_weight", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val new_weight = WEIGHT(tt, anno)
      gradMap(s) = ZEROS(tt, anno)
      momentumMap(s) = ZEROS(tt, anno)
      // FIXME(feiw) how do we deal with the case where multiple nodes are generated?
      new_weight.x

    case _ => super.transform(n)
  }

  override def transform(graph: Graph): Graph = {
    assert (g == null)
    g = new GraphBuilderOpt()
    Adapter.g = g

    // handle the metadata in OPERATION.resultMap
    OPERATION.oldResultMap = OPERATION.resultMap
    OPERATION.resultMap = new mutable.HashMap[lms.core.Backend.Exp, List[lms.core.Backend.Exp]]()

    try {
      super.transform(graph)
    } finally {
      g = null; Adapter.g = null
    }
  }
}
