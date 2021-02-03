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
  case class TensorType(shape: Seq[Size], et: Manifest[_], anno: Anno = NAnno) { // tensor type
    def namedDim(x: Int) = shape(x).dim
    def contains(d: Dim) = shape.map(_.dim).contains(d)
    def shapeSize = shape.map(_.size)
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
        case Some(Node(_, s, _, _)) => s.startsWith("tensor")
        case a => false
      }
    }
  }

  class TENSOR(override val x: Backend.Exp, override val useOldMetadata: Boolean = false) extends TOP(x) {
    def withEleType(m: Manifest[_]): this.type = { Adapter.typeMap(x) = m; this }
    override def withSrcType(pos: SourceContext, m: Manifest[_]): this.type =
      withSource(pos).withEleType(m)

    def pos: SourceContext = if (useOldMetadata) Adapter.oldSourceMap(x) else Adapter.sourceMap(x)

    def et: Manifest[_] = if (useOldMetadata) Adapter.oldTypeMap(x) else Adapter.typeMap(x)

    def tensor_type: TensorType = {
      gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, Backend.Const(tt:TensorType)::_, _)) if s.startsWith("tensor") => tt
        case a => throw new Exception(s"cannot find node $a")
      }
    }

    def annotation: Anno = {
      gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, tt::Backend.Const(a:Anno)::_, _)) if s.startsWith("tensor") => a
        case a => throw new Exception(s"cannot find node $a")
      }
    }

    def op_type = gc.get(x.asInstanceOf[Backend.Sym]) match {
      case Some(Node(_, s, _, _)) if s.startsWith("tensor") => s
      case a => throw new Exception(s"cannot find node $a")
    }

    def shape_size: Seq[Int] = tensor_type.shape.map(_.size)

    def show(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectEffect("show_tensor", x)(x)(Adapter.CTRL))
    }

    // FIXME(feiw) save to where?
    def save(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectEffect("save", x)(x)(Adapter.CTRL))
    }
  }

  class OPERATION(override val x: Backend.Exp, override val useOldMetadata: Boolean = false) extends TOP(x) {
    val (resultTypes: List[TensorType], annotation: Anno) = gc.get(x.asInstanceOf[Backend.Sym]) match {
      case Some(Node(_, s, Backend.Const(result_tensor_types: List[TensorType])::Backend.Const(anno:Anno)::_, _))
        if s.startsWith("op") => (result_tensor_types, anno)
      case a => throw new Exception(s"Node $a is not an operation node")
    }

    val numResults: Int = resultTypes.length

    // build and cache the tensor results of operation
    if (useOldMetadata) {
      require(OPERATION.oldResultMap.contains(x.asInstanceOf[Backend.Sym]),
        s"must already contain $x in the result map: ${OPERATION.oldResultMap}")
    } else {
      if (OPERATION.resultMap == null)
        OPERATION.resultMap = mutable.HashMap[lms.core.Backend.Exp, List[lms.core.Backend.Exp]]()
      OPERATION.resultMap.getOrElseUpdate(x.asInstanceOf[Backend.Sym], results.map(_.x))
    }

    def getResultType(i: Int): TensorType = {
      require(i >= 0 && i < numResults, s"parameter must be in Range of $numResults but got $i")
      resultTypes(i)
    }

    def getResult(i: Int): TENSOR = {
      require(i >= 0 && i < numResults, s"parameter must be in Range of $numResults but got $i")
      getResults(i)
    }

    def getResults: List[TENSOR] = if (useOldMetadata) {
      OPERATION.oldResultMap(x.asInstanceOf[Backend.Sym]).map(
        x => new TENSOR(x, useOldMetadata=true)
      )
    } else {
      OPERATION.resultMap(x.asInstanceOf[Backend.Sym]).map(x => new TENSOR(x))
    }

    private def result(i: Int): TENSOR = {
      require(i >= 0 && i < numResults, s"parameter must be in Range of $numResults but got $i")
      OPERATION.getResult(this, i, getResultType(i), annotation)
    }

    private def results: List[TENSOR] = (0 until numResults).toList.map(i => result(i))
  }

  object OPERATION {
    def getResult(x: OPERATION, i: Int, tensorType: TensorType, anno: Anno)(implicit __pos: SourceContext) = {
      (new TENSOR(Adapter.g.reflect("tensor_result", C(tensorType), C(anno), x.x, C(i)))).withSrcType(__pos, tensorType.et)
    }

    // We are using these global data structures to hold operation graph metadata
    var oldResultMap: mutable.Map[lms.core.Backend.Exp, List[lms.core.Backend.Exp]] = _
    var resultMap: mutable.Map[lms.core.Backend.Exp, List[lms.core.Backend.Exp]] = _
  }

  def MODULE(f: => TENSOR)(implicit __pos: SourceContext): Int => UNIT = {
    val m = Adapter.g.reflectWrite("module", Adapter.g.reify(f.x))(Adapter.CTRL)
    (iter: Int) => UNIT(Adapter.g.reflectWrite("@", m, Backend.Const(iter))(Adapter.CTRL))
  }

  def mergable_dims(node: Node): List[(Dim, Dim)] = node match {
    case Node(s, op, _, _) if (op == "tensor_input" || op == "tensor_weight" || op == "tensor_result") => List()
    case Node(s, op, _, _) =>
      assert(!op.startsWith("tensor"), s"node $node is not yet handled in mergable_dims")
      List()
  }

  def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
      weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit],
      gradMap: mutable.HashMap[Backend.Sym, TENSOR],
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

    def input[T:Manifest](tensor_type: TensorType)(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = INPUT(tensor_type, anno)
      Wrap[Tensor[T]](tensor.x)
    }

    def weight[T:Manifest](shape: Seq[Int], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = WEIGHT(shape, manifest[T], index, devices)
      Wrap[Tensor[T]](tensor.x)
    }

    def weight[T:Manifest](shape: Seq[Int])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = WEIGHT(TensorType(shape.map(s => Size(dim, s)), manifest[T]), anno)
      Wrap[Tensor[T]](tensor.x)
    }

    def ones[T:Manifest](tensor_type: TensorType)(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = ONES(tensor_type, anno)
      Wrap[Tensor[T]](tensor.x)
    }

    def zeros[T:Manifest](tensor_type: TensorType)(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = ZEROS(tensor_type, anno)
      Wrap[Tensor[T]](tensor.x)
    }
  }

  def tensor_type[T:Numeric:Manifest](shape: Seq[Int]): TensorType =
    TensorType(shape.map(s => Size(dim, s)), manifest[T])

  def tensor[T:Numeric:Manifest](x: Rep[Tensor[T]]): TENSOR = new TENSOR(Unwrap(x))

  implicit class TensorOps[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)
    def shape: Seq[Int] = self.shape_size
    def show(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](self.show.x)
  }
}

