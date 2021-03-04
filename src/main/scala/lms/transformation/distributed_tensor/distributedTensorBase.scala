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
  case class TensorType(shape: Seq[Size], et: Manifest[_], anno: Anno = NAnno, tensorName: Option[String] = None) { // tensor type
    def namedDim(x: Int) = shape(x).dim
    def contains(d: Dim) = shape.map(_.dim).contains(d)
    def shapeSize = shape.map(_.size)
    def shapeDim = shape.map(_.dim)
    def shapeSizeAfterSplit(d: Dim, degree: Int) = shape.map(s => if (s.dim == d) s.size / degree else s.size)
  }

  abstract class Anno extends Serializable // Annotation class
  object NAnno extends Anno { // Null Annotation
    override def toString = "NAnno"
  }
  case class SAnno(dim: Dim, devices: Seq[Device], stable: Boolean = true) extends Anno // spatial splitting annotation
  case class KAnno(pipeline: Int) extends Anno // stacked pipelines
  case class QAnno(pipeline: Int) extends Anno // queued pipelines

  def INPUT(shape: Seq[Int], et: Manifest[_], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): TENSOR = {
    val tensorType = TensorType(shape.map(s => Size(dim, s)), et) // this is using default NAnno
    val anno = SAnno(tensorType.namedDim(index), devices)
    INPUT(tensorType, anno)
  }

  def INPUT(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_input", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  def WEIGHT(shape: Seq[Int], et: Manifest[_], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): TENSOR = {
    val tensorType = TensorType(shape.map(s => Size(dim, s)), et) // this is using default NAnno
    val anno = SAnno(tensorType.namedDim(index), devices)
    WEIGHT(tensorType, anno)
  }

  def WEIGHT(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_weight", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  def ONES(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_ones", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  def ZEROS(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_zeros", C(tt), C(anno)))).withSrcType(__pos, tt.et)
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
      UNIT(Adapter.g.reflectEffect("save", x)(x)(Adapter.CTRL))
    }

    def check(filename: String)(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectEffect("check_tensor", x, lms.core.Backend.Const(filename))(x)(Adapter.CTRL))
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
  }

  def MODULE(f: => TENSOR)(implicit __pos: SourceContext): Int => UNIT = {
    val m = Adapter.g.reflectWrite("module", Adapter.g.reify(f.x))(Adapter.CTRL)
    (iter: Int) => UNIT(Adapter.g.reflectWrite("@", m, Backend.Const(iter))(Adapter.CTRL))
  }

  def mergable_dims(node: Node): List[(Dim, Dim)] = node match {
    case Node(s, op, _, _) if (op == "tensor_input" || op == "tensor_weight" || op == "tensor_result") => List()
    case Node(s, op, _, _) =>
      assert(!op.startsWith("tensor_"), s"node $node is not yet handled in mergable_dims")
      List()
  }

  // helper data structure for wrapping hashmap FIXME(feiw): we have to define it here :( for the aircopCollect function
  case class GradMapWrapper(map: scala.collection.mutable.HashMap[Backend.Sym, Backend.Sym]) {
    def apply(x: Backend.Exp): TENSOR = Adapter.oldDefsCache.get(x.asInstanceOf[Backend.Sym]) match {
      case Some(Node(_, "tensor_result", tt::anno::(op:Backend.Sym)::Backend.Const(i:Int)::_, _)) =>
        Adapter.g.globalDefsCache.get(map(op)) match {
          case Some(Node(_, "tuple-view", xs: List[Backend.Sym], _)) => new TENSOR(xs(i))
          case a => throw new Exception(s"$a is not a tuple view")
        }
      case n@Some(Node(_, op, _, _)) if op.startsWith("tensors_") => throw new Exception(s"$x is Tensors, not a Tensor: $n")
      case _ => new TENSOR(map(x.asInstanceOf[Backend.Sym]))
    }
    def getGradsOfOp(x: Backend.Exp): List[TENSOR] = Adapter.oldDefsCache.get(x.asInstanceOf[Backend.Sym]) match {
      case n@Some(Node(_, op, _, _)) if op.startsWith("tensors_") => Adapter.g.globalDefsCache.get(map(x.asInstanceOf[Backend.Sym])) match {
          case Some(Node(_, "tuple-view", xs: List[Backend.Sym], _)) => xs.map(new TENSOR(_))
          case a => throw new Exception(s"$a is not a tuple view")
        }
      case n => throw new Exception(s"$n is not an operation")
    }
    def update(x: Backend.Exp, grad: TENSOR) = { map(x.asInstanceOf[Backend.Sym]) = grad.x.asInstanceOf[Backend.Sym] }
    def update(x: Backend.Exp, grad: Backend.Exp) = { map(x.asInstanceOf[Backend.Sym]) = grad.asInstanceOf[Backend.Sym] }
  }

  def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
      weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit],
      gradMap: GradMapWrapper,
      momentumMap: mutable.HashMap[Backend.Sym, TENSOR],
      transform: Backend.Exp => Backend.Exp): Unit = node match {

    case Node(s, "tensor_input", _, _) => forwardNodes += node
    case Node(s, "tensor_weight", _, _) => weightNodes += node
    case Node(s, "tensor_result", _, _) => forwardNodes += node
    case Node(s, op, _, _) => throw new Exception(s"op $op is not yet handled in aircopCollect \n$node")
  }
}

trait FixedSizeDistributedTensorOpsBase extends Dsl {

  import PrimitiveTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  def module[T](f: => Rep[Tensor[T]])(implicit __pos: SourceContext) = {
    MODULE(new TENSOR(Unwrap(f)))
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

    def input[T:Manifest](shape: Seq[Int], name: String, splitDim: Int, splitTo: List[Device])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val tensorType = resultType[Float](shape, tensorName=Some(name))
      val sAnno = SAnno(tensorType.shape(splitDim).dim, splitTo)
      val tensor = INPUT(tensorType, sAnno)
      Wrap[Tensor[T]](tensor.x)
    }

    def weight[T:Manifest](shape: Seq[Int], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = WEIGHT(shape, manifest[T], index, devices)
      Wrap[Tensor[T]](tensor.x)
    }

    def weight[T:Manifest](shape: Seq[Int], tensorName: Option[String] = None)(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = WEIGHT(TensorType(shape.map(s => Size(dim, s)), manifest[T], tensorName = tensorName), anno)
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
    TensorType(shape.map(s => Size(dim, s)), manifest[T], tensorName=tensorName)

  def tensor[T:Numeric:Manifest](x: Rep[Tensor[T]]): TENSOR = new TENSOR(Unwrap(x))

  implicit class TensorOps[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)
    def shape: Seq[Int] = self.shapeSize
    def anno: Anno = self.annotation
    def show(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](self.show.x)
  }
}

