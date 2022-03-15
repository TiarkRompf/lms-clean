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

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  // Number of modules
  var moduleCount = 0
  val forwardNodes = mutable.ArrayBuffer[Node]()
  val weightNodes = mutable.ArrayBuffer[Node]()
  val backwardNodes = mutable.ArrayBuffer[()=>Unit]()
  // this is the set of all the lifted nodes
  val liftNodes = mutable.Set[Backend.Sym]()
  // this is the map from lifted tensor to their versioned array
  val versionMap = mutable.HashMap[Backend.Sym, TENSORARRAY]()
  // this is the gradient map from OLD value tensors to NEW gradient tensors
  val momentumMap = mutable.HashMap[Backend.Sym, TENSOR]()
  val gradMap_ = mutable.HashMap[Backend.Sym, Backend.Sym]()
  val gradMap = GradMapWrapper(gradMap_)

  // Need to clear module-related environment after traverse module
  def clearModuleEnv(): Unit = {
    forwardNodes.clear()
    weightNodes.clear()
    backwardNodes.clear()
    liftNodes.clear()
    versionMap.clear()
    momentumMap.clear()
    gradMap_.clear()
  }

  // Entry function to transform module
  def transformModule(module: MODULE, traverse: MODULE => (Seq[Node], Block) => Backend.Exp) = {
    clearModuleEnv()
    implicit val pos = module.pos
    val block = module.gc.get(module.x.asInstanceOf[Backend.Sym]) match {
      case Some(Node(m, "module", Backend.Const(manno:Anno)::(b@Block(in,res,ein,eff))::_, _)) => b
      case a => throw new Exception(s"Node $a is not an Module node")
    }
    if (moduleCount > 1) {
      val mBlock = Adapter.g.reify{
        scheduleBlock(block)(traverse(module))
      }
      UNIT(Adapter.g.reflectEffectSummary("module", C(module.annotation), mBlock)(Adapter.g.getEffKeys(mBlock))).x
    } else {
      scheduleBlock(block)(traverse(module))
    }
  }
  // This function is for training
  def traverseNonPipelineModule(iter: Int)(module: MODULE)(ns: Seq[Node], res: Block): Backend.Exp = {
    // Step 1: Collection Phase
    ns.foreach { n => aircopCollect(n, forwardNodes, weightNodes, backwardNodes, liftNodes, gradMap, momentumMap, transform) }
    // result of the block
    implicit val pos = Adapter.oldSourceMap(ns.last.n)
    implicit val md = module
    if (module.islastmodule) {
      (() => {
        val result = new TENSOR(transform(module.result.x))
        // FIXME(feiw) this should not be check, but be log.
        result.check("loss")
        val grad = ONES(result.resultType, result.annotation)
        gradMap(module.result.x) = grad
      }) +=: backwardNodes
    } else {
      (() => {
        val result = new TENSOR(transform(module.result.x))
        // FIXME(feiw) this should not be check, but be log.
        result.check("loss")
        val grad = ZEROS(result.resultType, result.annotation)
        // need to recv the gradient the previously sent result.
        val recv =  RECV(grad, module.result.x.toString, true)
        gradMap(module.result.x) = grad
      }) +=: backwardNodes
    }
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

  // This function is for stacked pipeline training
  def traverseStackedModule(iter: Int)(module: MODULE)(ns: Seq[Node], res: Block): Backend.Exp = {
    // Step 1: Collection Phase for stacked pipeline, need to collect liftNodes
    ns.foreach { n => aircopCollect(n, forwardNodes, weightNodes, backwardNodes, liftNodes, gradMap, momentumMap, transform) }
    implicit val pos = Adapter.oldSourceMap(ns.last.n)
    implicit val md = module
    // The result will be send to other module except for the last module

    if (module.islastmodule) {
      (() => {
        // only work when result is a single tensor, does not support multiple tensors
        val result = new TENSOR(transform(module.result.x))
        // FIXME(feiw) this should not be check, but be log.
        //result.check("loss")
        val grad = ONES(result.resultType, result.annotation)
        gradMap(module.result.x) = grad
      }) +=: backwardNodes
    } else {
      (() => {
        val result = new TENSOR(transform(module.result.x))
        // FIXME(feiw) this should not be check, but be log.
        //result.check("loss")
        val grad = ZEROS(result.resultType, result.annotation)
        // need to recv the gradient of the previously sent result.
        val recv =  RECV(grad, module.result.x.toString, true)
        gradMap(module.result.x) = grad
      }) +=: backwardNodes
    }
    // collect all weight syms and all forward syms
    val weightSyms = weightNodes.map(s => s.n)
    val forwardSyms = forwardNodes.map(s => s.n)
    liftNodes --= weightSyms.toSet


    // Step 2: Generation Phase for stacked pipeline
    traverseWeights(weightNodes) {
      traverseLiftNodes(liftNodes) {
        for (i <- (0 until iter/module.pipeline): Rep[Range]) {
          traverseStackedForward(forwardNodes) {
            traverseStackedBackward(backwardNodes, forwardSyms) {
              traverseOptimization(weightSyms) { checkGradients(weightSyms, true) }
            }
          }
        }
      }
    }

    // Todo: save the weight
    // FIXME(feiw) change res to updated weights
    //val updated_weights = weightSyms.map(w => subst(w)).toList
    //updated_weights.foreach(w => (new TENSOR(w)).save)
    Backend.Const(())
  }

  // this function is for testing
  def traverseNonPipelineModule(loss: String)(module: MODULE)(ns: Seq[Node], res: Block): Backend.Exp = {
    // Step 1: Collection Phase
    ns.foreach { n => aircopCollect(n, forwardNodes, weightNodes, backwardNodes, liftNodes, gradMap, momentumMap, transform) }
    implicit val pos = Adapter.oldSourceMap(ns.last.n)
    implicit val md = module
    (() => {
      val result = new TENSOR(transform(module.result.x))
      result.check(loss)
      val grad = ONES(result.resultType, result.annotation)
      gradMap(module.result.x) = grad
    }) +=: backwardNodes
    // collect all weight syms and all forward syms
    val weightSyms = weightNodes.map(s => s.n)
    val forwardSyms = forwardNodes.map(s => s.n)
    // remove input Syms that are int type
    val inputSyms = forwardNodes.filter {
      case Node(s, "tensor_input", Backend.Const(tt: TensorType)::_, _) => tt.et match {
        case a if a == manifest[Int] => false
        case _ => true
      }
      case _ => false
    }.map(s => s.n)

    // Step 2: Generation Phase
    traverseWeights(weightNodes) {
      traverseForward(forwardNodes) {
        traverseBackward(backwardNodes, forwardSyms) {
          checkGradients(weightSyms ++ inputSyms)
        }
      }
    }
    Backend.Const(())
  }

  def traverseLiftNodes(liftNodes: mutable.Set[Backend.Sym])(cont: => Unit)(implicit module: MODULE) = {
    for (l <- liftNodes) {
      implicit val pos = Adapter.oldSourceMap(l)
      val oldt = new TENSOR(l, true)
      // versioned array of tensor
      versionMap(l) =  ZEROSARRAY(oldt.resultType, oldt.annotation, module.pipeline)
    }
    cont
  }

  def traverseStackedForward(forwards: mutable.ArrayBuffer[Node])(cont: => Unit)(implicit module: MODULE) = {
    val res = module.result.x
    implicit val pos = Adapter.oldSourceMap(res)
    for (j <- (0 until module.pipeline): Rep[Range]) {
      for (f <- forwards) {
        implicit val pos = Adapter.oldSourceMap(f.n)
        traverse(f)
        if (liftNodes contains f.n) {
          // versioned node will not be mutated in forward pass, so we only need to set them once.
          TENSORARRAY.set(versionMap(f.n), INT(Unwrap(j)), new TENSOR(transform(f.n)))
        }
      }
      //new TENSOR(transform(module.result.x)).check("loss")
      if (!module.islastmodule) {
        implicit val pos = Adapter.oldSourceMap(res)
        // need to send the result to other module.
        SEND(new TENSOR(transform(res)), res.toString)
      }
      ()
    }
    cont
  }

  def traverseStackedBackward(backwards: mutable.ArrayBuffer[()=>Unit], forwardSyms: mutable.ArrayBuffer[Backend.Sym])(cont: => Unit)(implicit module: MODULE) = {
    implicit val pos = Adapter.oldSourceMap(module.result.x)
    for (j <- (0 until module.pipeline): Rep[Range]) {
      for (fs <- forwardSyms) {
        if ( fs != module.result.x) {
          implicit val pos = Adapter.oldSourceMap(fs)
          if (TENSOR.isTensor(transform(fs))) {
            val node = new TENSOR(transform(fs))
            val grad = ZEROS(node.resultType.map(_+"_grad"), node.annotation)
            gradMap(fs) = grad
          } else if (TENSORS.isTensors(transform(fs))) {
            val oldOp = new TENSORS(fs, useOldMetadata = true)
            val grads = oldOp.resultTypes.map(t=>ZEROS(t.map(_+"_grad"), oldOp.annotation))
            gradMap(fs) = TENSORS.tupleView(grads.map(_.x))
          }
        }
      }
      // versioned tensor will not be mutated in backwardpass, we only need to read them
      for (l <- liftNodes) {
        implicit val pos = Adapter.oldSourceMap(l)
        // Fixme: do we need to save old subst
        subst(l) = TENSORARRAY.get(versionMap(l), INT(Unwrap(j))).x
      }
      for (b <- backwards) {
        b()
      }
    }
    cont
  }

  def traverseWeights(weights: mutable.ArrayBuffer[Node])(cont: => Unit)(implicit module: MODULE) = {
    for (w <- weights) {
      implicit val pos = Adapter.oldSourceMap(w.n)
      traverse(w)
      val new_weight = new TENSOR(transform(w.n))
      val (tt, anno) = (new_weight.resultType, new_weight.annotation)
      gradMap(w.n) = ZEROS(tt.map(_+"_grad"), anno)
      momentumMap(w.n) = ZEROS(tt.map(_+"_momentum"), anno)
    }
    cont
  }

  def traverseForward(forwards: mutable.ArrayBuffer[Node])(cont: => Unit)(implicit module: MODULE) = {
    for (f <- forwards) {
      traverse(f)
    }
    if (!module.islastmodule) {
      implicit val pos = module.result.pos
      SEND(new TENSOR(transform(module.result.x)), module.result.x.toString)
    }
    cont
  }

  def traverseBackward(backwards: mutable.ArrayBuffer[()=>Unit], forwardSyms: mutable.ArrayBuffer[Backend.Sym])(cont: => Unit)(implicit module: MODULE) = {
    for (fs <- forwardSyms) {
      implicit val pos = Adapter.oldSourceMap(fs)

      if ( fs != module.result.x) {
        if (TENSOR.isTensor(transform(fs))) {
          val node = new TENSOR(transform(fs))
          val grad = ZEROS(node.resultType.map(_+"_grad"), node.annotation)
          gradMap(fs) = grad
        } else if (TENSORS.isTensors(transform(fs))) {
          val oldOp = new TENSORS(fs, useOldMetadata = true)
          val grads = oldOp.resultTypes.map(t=>ZEROS(t.map(_+"_grad"), oldOp.annotation))
          gradMap(fs) = TENSORS.tupleView(grads.map(_.x))
        }
      }
    }
    for (b <- backwards) {
      b()
    }
    cont
  }

  def traverseOptimization(weightSyms: mutable.ArrayBuffer[Backend.Sym])(cont: => Unit)(implicit module: MODULE) = {
    for (w <- weightSyms) {
      implicit val pos = Adapter.oldSourceMap(w)
      val annotation = (new TENSOR(w, useOldMetadata=true)).annotation
      Optimize(new TENSOR(transform(w)), gradMap(w), momentumMap(w), annotation)
    }
    cont
  }

  def checkGradients(weightSyms: => mutable.ArrayBuffer[Backend.Sym], check_weight: Boolean = false)(implicit module: MODULE) = {
    for (w <- weightSyms) {
      val weight = new TENSOR(transform(w))
      implicit val pos: SourceContext = weight.pos
      (weight.filenameFormat, weight.filenameArgs) match {
        case (Some(name), args) => {
          gradMap(w).check(name + "_grad", args:_*) // FIXME(feiw) unfortunately hard-coded
          if (check_weight)
            weight.check(name, args:_*)
        }
        case (None, _) => throw new Exception(s"Missing checked file names for tensor $weight")
      }
    }
  }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "module", Backend.Const(manno:Anno)::(b @ Block(in, y, ein, eff))::_, _) => Backend.Const(())

    case Node(s, "@", List(a: Backend.Sym, Backend.Const(iter: Int)), _) => {
      // the `iter: Int` means that it is the training call with `iter` iterations
      Adapter.oldDefsCache(a) match {
        case Node(s, "module", Backend.Const(manno:MAnno)::(b @ Block(in, y, ein, eff))::_, _) => transformModule(new MODULE(s, true), traverseNonPipelineModule(iter))
        case Node(s, "module", Backend.Const(manno:KAnno)::(b @ Block(in, y, ein, eff))::_, _) => transformModule(new MODULE(s, true), traverseStackedModule(iter))
        case Node(s, "module", Backend.Const(manno:QAnno)::(b @ Block(in, y, ein, eff))::_, _) => throw new Exception(s"Queued pipeline not supported")
        case _ => super.transform(n)
      }
    }

    case Node(s, "@", List(a: Backend.Sym, Backend.Const(loss: String)), _) =>
      // the `loss: String` means that it is the test call with `loss` being the filename of the expect loss values
      Adapter.oldDefsCache(a) match {
        case Node(s, "module", Backend.Const(manno @ MAnno(devices, true))::(b @ Block(in, y, ein, eff))::_, _) => transformModule(new MODULE(s, true), traverseNonPipelineModule(loss))
        case Node(s, "module", Backend.Const(manno @ MAnno)::(b @ Block(in, y, ein, eff))::_, _) => throw new Exception(s"Multiple module needs training loop")
        case Node(s, "module", Backend.Const(manno:KAnno)::(b @ Block(in, y, ein, eff))::_, _) => throw new Exception(s"Stacked pipeline needs training loop")
        case Node(s, "module", Backend.Const(manno:QAnno)::(b @ Block(in, y, ein, eff))::_, _) => throw new Exception(s"Queued pipeline needs training loop")
        case _ => super.transform(n)
      }

    case Node(s, "tensor_module", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(x:Backend.Sym)::_, _) => {
      implicit val pos = Adapter.oldSourceMap(s)
      val tag = new MODULE(x, true).result.x.toString
      val recv = ZEROS(tt, anno)
      RECV(recv,tag)
      recv.x
    }

    case _ => super.transform(n)
  }

  override def transform(graph: Graph): Graph = {
    assert (g == null)
    g = new GraphBuilderOpt()
    Adapter.g = g
    moduleCount = graph.nodes.foldLeft(0)((num, n) => n match {
      case Node(s, "module", _, _) => num + 1
      case _ => num
    })

    try {
      super.transform(graph)
    } finally {
      g = null; Adapter.g = null
    }
  }
}
