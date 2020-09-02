package lms
package transformation

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.ArrayCPUOps

import Backend._

/**
 * In this frontend design, we are showing a paradigm of building both the `typeless`
 * frontend (with all captilized letters: TENSOR), and the `typed` frontend (Tensor).
 *
 * Both typeless frontend and typed frontend will wrap `Backend.Exp` with Scala classes
 * and register the MetaData (including SourceContext and type Manifest).
 *
 * In this given design, the typed frontend actually uses the typeless frontend internally.
 * That is to say, the typed frontend is a shallow wrapper of the typeless frontend.
 * This is not super necessary but it should avoid duplication of logic in IR construction.
 *
 * We are trying to bring up the typeless frontend in other frontend traits as well, because
 * the typeless frontend is more friendly to IR transformations.
 * This is similar to how MLIR is typeless and has been easy to run transformations.
 */
trait FixedSizeTensorFrontEnd extends Base with PrimitiveOps with ArrayOps {

  /// typeless frontend
  // Note how the SourceContext and type Manifest are registered.
  // The `case class TENSOR` is used for `unsafe wrapping` (wrapping Backend.Exp as TENSOR)
  //      and the methods associated with TENSOR
  // The `def TENSOR` is used for `safe construction` of TENSOR, which registers metadata.
  type E = Backend.Exp
  def C(a: Any) = Backend.Const(a)

  case class TENSOR(x: E) {
    def withSource(pos: SourceContext) = { Adapter.sourceMap(x) = pos; this }
    def withEleType(m: Manifest[_]) = { Adapter.typeMap(x) = m; this }
    def with_(pos: SourceContext, m: Manifest[_]) = withSource(pos).withEleType(m)

    def p: SourceContext = Adapter.sourceMap.getOrElse(x, ???)
    def et: Manifest[_] = Adapter.typeMap.getOrElse(x, ???)

    def shape: Seq[Int] = shape(Adapter.g.globalDefsCache)
    def shape(graphCache: Map[Backend.Sym, Backend.Node]): Seq[Int] = {
      graphCache.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, Backend.Const(size:Seq[Int])::_, _)) if s.startsWith("tensor") => size
        case a => System.out.println(a); ???
      }
    }

    def show(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectWrite("show_tensor", x)(Adapter.CTRL))
    }

    def + (y: TENSOR)(implicit __pos: SourceContext): TENSOR = {
      assert(shape == y.shape)
      assert(et == y.et)
      TENSOR(Adapter.g.reflect("tensor_add", C(shape), x, y.x)).with_(__pos, et)
    }
  }

  def TENSOR(shape: Seq[Int], array: ARRAY)(implicit __pos: SourceContext): TENSOR = {
    TENSOR(Adapter.g.reflect("tensor", C(shape), array.x)).with_(__pos, array.et)
  }


  /// typed frontend
  // Note how the typed frontend shallowly wrap the typeless frontend.
  // The `object Tensor` is used to construct a Rep[Tensor[T]]
  // The `implicit class TensorOps` is used to provide methods of Rep[Tensor[T]]
  class Tensor[+T]
  object Tensor {
    def apply[T:Numeric:Manifest](shape: Seq[Int], array: Rep[Array[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      // Whenever we have something that is Rep[T], it is OK to use `unsafe construction` (CLASS(Unwrap(a)))
      // to obtain the typeless object because the metadata must have been registered.

      // Then we use the safe construction of TENSOR to build TENSOR object, which is then
      // shallowly wrapped to Rep[Tensor[T]]
      Wrap[Tensor[T]](TENSOR(shape, ARRAY(Unwrap(array))).x)
    }
  }

  implicit class TensorOps[T:Numeric:Manifest](x: Rep[Tensor[T]]) {

    // first keep a local instance of TENSOR. No need to worry about registering metadata
    // here because we know that the metadata has ben registered when x: Rep[Tensor[T]] is constructed
    val self = TENSOR(Unwrap(x))

    def shape: Seq[Int] = self.shape
    def show(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](self.show.x)
    def + (y: Rep[Tensor[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self + TENSOR(Unwrap(y))
      Wrap[Tensor[T]](t.x)
    }
  }
}

// lower Tensor computations to Array computations
abstract class TensorLoweringCPU extends Transformer with ArrayCPUOps with FixedSizeTensorFrontEnd {

  // need a global mapping from Tensor to Array
  val tensor2array = new mutable.HashMap[Backend.Sym, Backend.Sym]

  def numeral(size: Seq[Int]) = size.foldLeft(1)(_ * _)

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::_, _) =>
      tensor2array(s) = transform(x).asInstanceOf[Backend.Sym]
      s
    case Node(s, "tensor_add", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      // `oldSourceMap` is the copy of Adapter.sourceMap that maps Backend.Exp to SourceContext
      // We use it to get the SourceContext of the node `s` and use it for the transformed node.
      implicit val sc_ : SourceContext = oldSourceMap(s)

      // `oldTypeMap` is the copy of Adapter.typeMap that maps Backend.Exp to type Manifest
      // We use it to get the type Manifest that is used for typeless frontend (such as for NEW_ARRAY)

      // Note that when construction new IR nodes from the old IR nodes, we choose
      // to use the typeless frontend because typeless frontend doesn't have to revive the type.
      // It can work with type manifest directly.
      val res = NEW_ARRAY(numeral(size), oldTypeMap(s))
      // add this new array to the `tensor2array` hashmap
      tensor2array(s) = res.x.asInstanceOf[Backend.Sym]

      // Again, we use typeless frontend here
      // We can also use the `unsafe constructor` of `ARRAY` because all arrays metapipe are already registered.
      ARRAY_ADD(ARRAY(tensor2array(x)), ARRAY(tensor2array(y)), res, INT(numeral(size)))
      res.x

    case Node(s, "show_tensor", (x: Backend.Sym)::Nil, _) =>
      implicit val sc_ = oldSourceMap(s)

      // this unsafe TENSOR construction should be safe because the TENSOR x is already constructed with metadata
      val tensor = TENSOR(x)
      // FIXME(feiw) need to provide graphCache for fetching the shape
      val shape = tensor.shape(graphCache)

      // this unsafe ARRAY construction should be safe because the ARRAY is already constructed with metadata
      val arr = ARRAY(tensor2array(x))

      // Using typeless frontend for printing
      ARRAY_PRINT(arr, INT(numeral(shape)))

      Backend.Const(())

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
