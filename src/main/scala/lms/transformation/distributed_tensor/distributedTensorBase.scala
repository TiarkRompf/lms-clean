package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

/**
 * In this frondend design, we are building Tensor IR with fixed shape and device.
 *
 * In the first step, we are simply supporting GPU and CPU.
 * Both Tensor IR and Tensor computation IR have device attributes.
 * We hope to resolve tensor communication automatically during transformation.
 *    such that: tensors are moved to GPU or CPU based on the request from tensor computation
 *               tensors can be printed only from CPU.
 */
trait FixedSizeDistributedTensorBaseTypeLess {

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._

  type E = Backend.Exp
  def C(a: Any) = Backend.Const(a)

  abstract class Device
  object UnKnownD extends Device
  case class CPU(x: Int) extends Device
  case class GPU(x: Int) extends Device

  var dim_name: Int = 0;
  def next_dim_name: Int = {
    dim_name += 1
    dim_name - 1
  }

  case class Dim(x: Int) // named dimension
  def dim = Dim(next_dim_name) // fresh dim
  case class Size(dim: Dim, size: Int) // dim and length
  case class TensorType(shape: Seq[Size], et: Manifest[_]) { // tensor type
    def namedDim(x: Int) = shape(x).dim
    def contains(d: Dim) = shape.map(_.dim).contains(d)
    def shapeSize = shape.map(_.size)
    def shapeSizeToString = shape.map(ss=> s"${ss.size}d${ss.dim.x}").toList.mkString("x")
    // FIXME(feiw): can this be done better, such as using super/subscript for dim name??
    // def shapeSizeToString = shape.map(ss=> s"${ss.size}<sub>${ss.dim.x}</sub>").toList.mkString("x")
    def shapeDim = shape.map(_.dim)
    def shapeSizeAfterSplit(d: Dim, degree: Int) = shape.map(s => if (s.dim == d) s.size / degree else s.size)
    def map(f: String => String) = TensorType(shape, et)
    def etToString = et match {
      case et if et == manifest[Boolean] => "b8"
      case et if et == manifest[Char] => "i8"
      case et if et == manifest[Int] => "i32"
      case et if et == manifest[Long] => "i64"
      case et if et == manifest[Float] => "f32"
      case et if et == manifest[Double] => "f64"
      case et => throw new Exception(s"implement the string representation of ${et}")
    }
    override def toString = s"<${shapeSizeToString}x${etToString}>"
    def splitBy(anno:Anno) = anno match {
      case NAnno => this
      case SAnno(dim: Dim, devices: Seq[Device], _) if contains(dim) =>
        TensorType(shape.map(s=>if(s.dim==dim) Size(dim, s.size/devices.length) else s), et)
      case SAnno(dim: Dim, devices: Seq[Device], _) if !contains(dim) => this
    }
  }

  abstract class Anno extends Serializable // Annotation class
  object NAnno extends Anno { // Null Annotation
    override def toString = "NAnno"
  }
  // spatial splitting annotation
  case class SAnno(dim: Dim, devices: Seq[Device], stable: Boolean = true) extends Anno {
    override def toString = s"Split d${dim.x} at devices=[${devices.mkString(", ")}]"
  }
  case class MAnno(devices: Seq[Device], islastmodule: Boolean = false) extends Anno {
    override def toString = s"Non-pipeline module at devices=[${devices.mkString(", ")}]"
  } // non pipeline multiple modules
  case class KAnno(pipeline: Int, devices: Seq[Device], islastmodule: Boolean = false) extends Anno {
    override def toString = s"Stacked pipeline at ${pipeline} at devices=[${devices.mkString(", ")}]"
  } // stacked pipelines
  case class QAnno(pipeline: Int, devices: Seq[Device], islastmodule: Boolean = false) extends Anno {
    override def toString = s"Queued pipeline at ${pipeline} at devices=[${devices.mkString(", ")}]"
  } // queued pipelines

  def INPUT(shape: Seq[Int], et: Manifest[_], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): TENSOR = {
    val tensorType = TensorType(shape.map(s => Size(dim, s)), et) // this is using default NAnno
    val anno = SAnno(tensorType.namedDim(index), devices)
    INPUT(tensorType, anno)
  }

  def INPUT(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_input", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  def INPUT(tt: TensorType, anno: Anno, filenameFormat: String, filenameArgs: TOP*)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_input", C(tt)::C(anno)::Backend.Const(filenameFormat)::filenameArgs.map(_.x).toList:_*))).withSrcType(__pos, tt.et)
  }

  def WEIGHT(shape: Seq[Int], et: Manifest[_], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): TENSOR = {
    val tensorType = TensorType(shape.map(s => Size(dim, s)), et) // this is using default NAnno
    val anno = SAnno(tensorType.namedDim(index), devices)
    WEIGHT(tensorType, anno)
  }

  def WEIGHT(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_weight", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  def WEIGHT(tt: TensorType, anno: Anno, filenameFormat: String, filenameArgs: TOP*)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_weight", C(tt)::C(anno)::Backend.Const(filenameFormat)::filenameArgs.map(_.x).toList:_*))).withSrcType(__pos, tt.et)
  }

  def ONES(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_ones", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  def ZEROS(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_zeros", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  def MODULE(anno:Anno, f: => TENSOR)(implicit __pos: SourceContext): MODULE = {
    new MODULE(Adapter.g.reflectWrite("module", C(anno), Adapter.g.reify(f.x))(Adapter.CTRL)).withSource(__pos)
  }

  val moduleTensorMap = new mutable.HashMap[Sym, Exp]

  // Typeless MOUDULE Definition
  class MODULE(override val x: Backend.Exp, override val useOldMetadata: Boolean = false) extends TOP(x, useOldMetadata) {
    def withEleType(m: Manifest[_]): this.type = { Adapter.typeMap(x) = m; this }
    override def withSrcType(pos: SourceContext, m: Manifest[_]): this.type =
      withSource(pos).withEleType(m)

    def pos: SourceContext = if (useOldMetadata) Adapter.oldSourceMap(x) else Adapter.sourceMap(x)

    def et: Manifest[_] = if (useOldMetadata) Adapter.oldTypeMap(x) else Adapter.typeMap(x)

    val (pipeline, annotation, devices, islastmodule) = gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, "module", Backend.Const(manno:Anno)::(b:Block)::_, _)) => manno match {
          case a @ NAnno => throw new Exception(s"Annotation $a is not an Module Annotation")
          case a @ SAnno(_,_,_) => throw new Exception(s"Annotation $a is not an Module Annotation")
          case a @ MAnno(devices, islastmodule) => (1, a, devices, islastmodule)
          case a @ KAnno(pipeline, devices, islastmodule) => (pipeline, a, devices, islastmodule)
          case a @ QAnno(pipeline, devices, islastmodule) => throw new Exception(s"Queued pipeline currently not supported")
          case a => throw new Exception(s"Annotation $a is not supported")
        }
        case m => throw new Exception(s"Node $m is not a Module node")
    }

    def train(iter: Int)(implicit __pos: SourceContext) = {
      UNIT(Adapter.g.reflectWrite("@", x, Backend.Const(iter))(Adapter.CTRL))
    }

    def test(lossName: String = "loss")(implicit __pos: SourceContext) = {
      UNIT(Adapter.g.reflectWrite("@", x, Backend.Const(lossName))(Adapter.CTRL))
    }

    val result = {
      gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(m, "module", Backend.Const(manno:Anno)::(b@Block(in,res,ein,eff))::_, _)) =>
          gc.get(res.asInstanceOf[Backend.Sym]) match {
            case Some(Node(res, s, Backend.Const(ott:TensorType)::Backend.Const(oanno:Anno)::_, _)) if s.startsWith("tensor_") => new TENSOR(res, useOldMetadata)
            case a => throw new Exception(s"Node $a is not an Tensor node")
          }
        case a => throw new Exception(s"Node $a is not an Module node")
      }
    }

    def getTensor(anno: Anno)(implicit __pos: SourceContext) = {
      val rt = result.resultType
      new TENSOR(moduleTensorMap.getOrElseUpdate(x.asInstanceOf[Backend.Sym], Adapter.g.reflectRead("tensor_module", C(rt), C(anno), x)(x))).withSrcType(__pos, rt.et)
    }
  }

  object MODULE {
    def TENSORMODULE(m:Backend.Exp, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
      // Fixme: need to create new tensortype with new dims?
       new MODULE(m).getTensor(anno)
      // val rt: TensorType = TensorType(resTensor.resultType.shape.map(s => Size(dim, s.size)), resTensor.resultType.et)
    }

    def getModuleTensor(x: Backend.Exp, useOldMetadata: Boolean = false) = {
      val gc = if (useOldMetadata) Adapter.oldDefsCache else Adapter.g.globalDefsCache
      gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(x, "tensor_module", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(m:Backend.Sym)::_, _)) => {
          new MODULE(m, useOldMetadata).result
        }
        case a => throw new Exception(s"Node $a is not an tensor_module node")
      }
    }
  }

  object TENSOR {
    def isTensor(x: Backend.Exp, useOldMetadata: Boolean = false) = {
      val gc = if (useOldMetadata) Adapter.oldDefsCache else Adapter.g.globalDefsCache
      gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, _, _)) => s.startsWith("tensor_") && s != "tensor_result"
        case a => false
      }
    }
  }

  class TENSOR(override val x: Backend.Exp, override val useOldMetadata: Boolean = false) extends TOP(x, useOldMetadata) {
    def withEleType(m: Manifest[_]): this.type = { Adapter.typeMap(x) = m; this }
    override def withSrcType(pos: SourceContext, m: Manifest[_]): this.type =
      withSource(pos).withEleType(m)

    def pos: SourceContext = if (useOldMetadata) Adapter.oldSourceMap(x) else Adapter.sourceMap(x)

    def et: Manifest[_] = if (useOldMetadata) Adapter.oldTypeMap(x) else Adapter.typeMap(x)

    val (resultType: TensorType, annotation: Anno) = gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _))
          if s.startsWith("tensor_") => (tt, anno)
        case a => throw new Exception(s"Node $a is not a Tensor node")
      }

    val shapeSize: Seq[Int] = resultType.shape.map(_.size)

    def show(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectEffect("show_tensor", x)(x)(Adapter.CTRL))
    }

    // FIXME(feiw) save to where?
    def save(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectEffect("save_tensor", x)(x)(Adapter.CTRL))
    }

    def check(filenameFormat: String, xs: TOP*)(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectEffect("check_tensor", x::Backend.Const(filenameFormat)::xs.map(_.x).toList:_*)(x)(Adapter.CTRL))
    }

    lazy val (filenameFormat: Option[String], filenameArgs: List[TOP]) = gc.get(x.asInstanceOf[Backend.Sym]) match {
      case Some(Node(_, s, Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(format:String)::(args:List[Backend.Exp]), _))
        if (s == "tensor_input" || s == "tensor_weight") => (Some(format), args.map(new TOP(_)))
      case _ => (None, Nil)
    }
  }

  class TENSORS(override val x: Backend.Exp, override val useOldMetadata: Boolean = false) extends TOP(x, useOldMetadata) {
    val (resultTypes: List[TensorType], annotation: Anno) = gc.get(x.asInstanceOf[Backend.Sym]) match {
      case Some(Node(_, s, Backend.Const(tts: List[TensorType])::Backend.Const(anno:Anno)::_, _))
        if s.startsWith("tensors_") => (tts, anno)
      case a => throw new Exception(s"Node $a is not an Tensors node")
    }

    val numResults: Int = resultTypes.length

    def getResultType(i: Int): TensorType = {
      require(i >= 0 && i < numResults, s"parameter must be in Range of $numResults but got $i")
      resultTypes(i)
    }
  }

  object TENSORS {
    def getResult(x: TENSORS, i: Int)(implicit __pos: SourceContext) = {
      require(i >= 0 && i < x.numResults, s"parameter must be in Range of ${x.numResults} but got $i")
      (new TENSOR(Adapter.g.reflect("tensor_result", C(x.getResultType(i)), C(x.annotation), x.x, C(i)))).withSrcType(__pos, x.getResultType(i).et)
    }

    def isTensors(x: Backend.Exp, useOldMetadata: Boolean = false) = {
      val gc = if (useOldMetadata) Adapter.oldDefsCache else Adapter.g.globalDefsCache
      gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, _, _)) => s.startsWith("tensors_")
        case a => false
      }
    }

    def tupleView(xs: List[Backend.Exp]): Backend.Exp = Adapter.g.reflect("tuple-view", xs: _*)
    def handleTupleView[T](n: Option[Backend.Node])(func: (List[Backend.Sym]) => T): T = n match {
      case Some(Node(_, "tuple-view", xs: List[Backend.Sym], _)) => func(xs)
      case _ => throw new Exception(s"$n is not a tuple view")
    }
  }

  def mergable_dims(node: Node): List[(Dim, Dim)] = node match {
    case Node(s, op, _, _) if (op == "tensor_input" || op == "tensor_weight" || op == "tensor_result") => List()
    case Node(s, "tensor_module", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(x:Backend.Sym)::_, _) => {
      val oldt = MODULE.getModuleTensor(s, true)
      val newt = new TENSOR(s, true)
      val old_type = oldt.resultType
      val new_type = newt.resultType
      assert(oldt.shapeSize == newt.shapeSize)
      assert(oldt.et == newt.et)
      (old_type.shape zip new_type.shape).toList map { case (a:Size, b:Size) => (a.dim, b.dim) }
    }
    case Node(s, op, _, _) =>
      assert(!op.startsWith("tensor_"), s"node $node is not yet handled in mergable_dims")
      List()
  }

  // helper data structure for wrapping hashmap FIXME(feiw): we have to define it here :( for the aircopCollect function
  case class GradMapWrapper(map: scala.collection.mutable.HashMap[Backend.Sym, Backend.Sym]) {
    def apply(x: Backend.Exp): TENSOR = Adapter.oldDefsCache.get(x.asInstanceOf[Backend.Sym]) match {
      case Some(Node(_, "tensor_result", tt::anno::(op:Backend.Sym)::Backend.Const(i:Int)::_, _)) =>
        TENSORS.handleTupleView(Adapter.g.globalDefsCache.get(map(op)))(xs => new TENSOR(xs(i)))
      case n@Some(Node(_, op, _, _)) if op.startsWith("tensors_") => throw new Exception(s"$x is Tensors, not a Tensor: $n")
      case _ => new TENSOR(map(x.asInstanceOf[Backend.Sym]))
    }
    def getGradsOfOp(x: Backend.Exp): List[TENSOR] = Adapter.oldDefsCache.get(x.asInstanceOf[Backend.Sym]) match {
      case n@Some(Node(_, op, _, _)) if op.startsWith("tensors_") =>
        TENSORS.handleTupleView(Adapter.g.globalDefsCache.get(map(x.asInstanceOf[Backend.Sym])))(xs => xs.map(new TENSOR(_)))
      case n => throw new Exception(s"$n is not an operation")
    }
    def update(x: Backend.Exp, grad: Backend.Exp): Unit = Adapter.oldDefsCache.get(x.asInstanceOf[Backend.Sym]) match {
      case Some(Node(_, "tensor_result", tt::anno::(op:Backend.Sym)::Backend.Const(i:Int)::_, _)) =>
        TENSORS.handleTupleView(Adapter.g.globalDefsCache.get(map(op))){ xs =>
          map(op) = TENSORS.tupleView(xs.updated(i, grad)).asInstanceOf[Backend.Sym]
        }
      case _ => map(x.asInstanceOf[Backend.Sym]) = grad.asInstanceOf[Backend.Sym]
    }
    def update(x: Backend.Exp, grad: TENSOR): Unit = update(x, grad.x)
  }

  // There will be no mutation to versioned variable in backward operation
  def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
      weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit], liftNodes: mutable.Set[Backend.Sym],
      gradMap: GradMapWrapper,
      momentumMap: mutable.HashMap[Backend.Sym, TENSOR],
      transform: Backend.Exp => Backend.Exp): Unit = node match {
    // Fixme: not dealing with tensors

    case Node(s, "tensor_input", _, _) => forwardNodes += node
    case Node(s, "tensor_weight", _, _) => weightNodes += node
    case Node(s, "tensor_result", _, _) => forwardNodes += node
    case Node(s, op, _, _) => throw new Exception(s"op $op is not yet handled in aircopCollect \n$node")
  }

  def printTensor(node: Node, graph: Graph): String = node match {
    case Node(s, "tensor_input", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(filenameFormat:String)::(filenameArgs:List[Backend.Exp]), _) =>
      s"$s = tensor_input(filenameFormat=${filenameFormat}, filenameArgs=[${filenameArgs.mkString(", ")}]) -> ${tt.toString}${if (anno != NAnno) s"  Anno: $anno" else ""}"
    case Node(s, "tensor_input", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      s"$s = tensor_input() -> ${tt.toString}${if (anno != NAnno) s"  Anno: $anno" else ""}"
    case Node(s, "tensor_weight", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(filenameFormat:String)::(filenameArgs:List[Backend.Exp]), _) =>
      s"$s = tensor_weight(filenameFormat=${filenameFormat}, filenameArgs=[${filenameArgs.mkString(", ")}]) -> ${tt.toString}${if (anno != NAnno) s"  Anno: $anno" else ""}"
    case Node(s, "tensor_weight", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      s"$s = tensor_weight() -> ${tt.toString}${if (anno != NAnno) s"  Anno: $anno" else ""}"
    case Node(s, "tensor_result", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(x:Backend.Sym)::Backend.Const(i:Int)::_, _) =>
      s"$s = ${x}#${i} -> ${tt.toString}${if (anno != NAnno) s"  Anno: $anno" else ""}"
    case Node(s, "tensor_module", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(x:Backend.Sym)::_, _) =>
      s"$s = tensor_module() -> ${tt.toString}${if (anno != NAnno) s"  Anno: $anno" else ""}"
    case Node(s, "tensor_zeros", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      s"$s = tensor_zeros() -> ${tt.toString}${if (anno != NAnno) s"  Anno: $anno" else ""}"
    case Node(s, "tensor_ones", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      s"$s = tensor_ones() -> ${tt.toString}${if (anno != NAnno) s"  Anno: $anno" else ""}"
    case Node(s, "check_tensor", (x:Backend.Sym)::Backend.Const(filenameFormat:String)::(filenameArgs:List[Backend.Exp]), _) =>
      s"$s = check_tensor($x, filenameFormat=$filenameFormat, filenameArgs=[${filenameArgs.mkString(", ")}]) ${symTensorShape(x, graph)}"
    case n => n.toString
  }

  def symTensorShape(x: Backend.Sym, graph: Graph): String = graph.globalDefsCache.get(x) match {
    case Some(Node(s, op, Backend.Const(tt:TensorType)::_, _)) if op.startsWith("tensor_") => tt.toString
    case _ => throw new Exception(s"sym $x is not a Tensor in the given graph ${graph}")
  }
}

trait FixedSizeDistributedTensorOpsBase extends Dsl {

  import PrimitiveTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  def showTensor(node: Node, graph: Graph): String = printTensor(node, graph)

  /// Typed Module Frontend
  class Module[+T]
  def module[T:Manifest](f: => Rep[Tensor[T]])(implicit __pos: SourceContext) = {
    Wrap[Module[T]](MODULE(MAnno(List(), islastmodule = true), new TENSOR(Unwrap(f))).x)
  }

  def module[T:Manifest](anno: Anno)(f: => Rep[Tensor[T]])(implicit __pos: SourceContext) = {
    Wrap[Module[T]](MODULE(anno, new TENSOR(Unwrap(f))).x)
  }

  implicit def myModule2res[T:Manifest](x: Rep[Module[T]])(implicit anno: Anno, __pos: SourceContext) : Rep[Tensor[T]] = {
    Wrap[Tensor[T]](module(x).getTensor(anno).x)
  }

  def module[T:Manifest](x: Rep[Module[T]]): MODULE = new MODULE(Unwrap(x))

  implicit class ModuleOps[T:Manifest](x: Rep[Module[T]]) {
    val self = module(x)
    val anno: Anno = self.annotation
    def train(iter: Int)(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](self.train(iter).x)
    def test(lossName: String = "loss")(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](self.test(lossName).x)
  }

  /// Typed Frontend
  class Tensor[+T]
  object Tensor {
    def input[T:Manifest](shape: Seq[Int], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = INPUT(shape, manifest[T], index, devices)
      Wrap[Tensor[T]](tensor.x)
    }

    def input[T:Manifest](shape: Seq[Int])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = INPUT(TensorType(shape.map(s => Size(dim, s)), manifest[T]), anno)
      Wrap[Tensor[T]](tensor.x)
    }

    def input[T:Manifest](resultType: TensorType)(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = INPUT(resultType, anno)
      Wrap[Tensor[T]](tensor.x)
    }

    def input[T:Manifest:Numeric](shape: Seq[Int], name: String, splitDim: Int, splitTo: List[Device])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val tensorType = resultType[T](shape)
      val sAnno = SAnno(tensorType.shape(splitDim).dim, splitTo)
      val tensor = INPUT(tensorType, sAnno, name)
      Wrap[Tensor[T]](tensor.x)
    }

    def weight[T:Manifest](shape: Seq[Int], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = WEIGHT(shape, manifest[T], index, devices)
      Wrap[Tensor[T]](tensor.x)
    }

    def weight[T:Manifest](shape: Seq[Int], tensorName: Option[String] = None)(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = tensorName match {
        case Some(name) => WEIGHT(TensorType(shape.map(s => Size(dim, s)), manifest[T]), anno, name)
        case None => WEIGHT(TensorType(shape.map(s => Size(dim, s)), manifest[T]), anno)
      }
      Wrap[Tensor[T]](tensor.x)
    }

    def ones[T:Manifest](resultType: TensorType)(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = ONES(resultType, anno)
      Wrap[Tensor[T]](tensor.x)
    }

    def zeros[T:Manifest](resultType: TensorType)(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = ZEROS(resultType, anno)
      Wrap[Tensor[T]](tensor.x)
    }
  }

  def resultType[T:Numeric:Manifest](shape: Seq[Int], tensorName: Option[String] = None): TensorType =
    TensorType(shape.map(s => Size(dim, s)), manifest[T])

  def tensor[T:Numeric:Manifest](x: Rep[Tensor[T]]): TENSOR = new TENSOR(Unwrap(x))

  implicit class TensorOps[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)
    def shape: Seq[Int] = self.shapeSize
    def anno: Anno = self.annotation
    def show(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](self.show.x)
  }
}
