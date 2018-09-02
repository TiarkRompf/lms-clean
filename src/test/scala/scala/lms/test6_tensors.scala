package scala.lms

import scala.annotation.implicitNotFound

import scala.collection.{mutable,immutable}
import Backend._

class TensorFrontEnd extends FrontEnd {

  case class SEQ(x: Exp) {
    def apply(y: INT): INT = INT(g.reflect("seq_apply",x,y.x))
    def explode(n: Int): List[INT] = (0 until n).map(i => apply(i)).toList
    def length: INT = INT(g.reflect("seq_length",x))
  }
  def SEQ(x: INT*): SEQ = SEQ(g.reflect("seq", x.toList.map(_.x):_*))

  case class Tensor(x: Exp) {
    def shape: SEQ = SEQ(g.reflect("tensor_shape", x))
    def apply(y: INT*): INT = apply(SEQ(y:_*))
    def apply(y: SEQ): INT = INT(g.reflect("tensor_apply",x,y.x))

    def +(y: Tensor): Tensor = Tensor(g.reflect("tensor_add",x,y.x))
  }

  def Tensor(shape: SEQ)(f: SEQ => INT): Tensor = Tensor(g.reflect("tensor", shape.x, g.reify(xn => f(SEQ(xn)).x)))
  def Tensor(shape: INT*)(f: SEQ => INT): Tensor = Tensor(SEQ(shape:_*))(f)

  def zeros(sh: SEQ): Tensor = Tensor(g.reflect("tensor_zeros",sh.x))
  def ones(sh: SEQ): Tensor = Tensor(g.reflect("tensor_ones",sh.x))
  def rand(sh: SEQ): Tensor = Tensor(g.reflect("tensor_rand",sh.x))

  def same_shape(a: SEQ, b: SEQ): SEQ = SEQ(g.reflect("tensor_same_shape",a.x,b.x))

  def Sum(shape: SEQ)(f: SEQ => INT): INT = INT(g.reflect("sum", shape.x, g.reify(xn => f(SEQ(xn)).x)))

  def PRINT(x: SEQ): Unit =
    g.reflectEffect("P",x.x)
  def PRINT(x: Tensor): Unit =
    g.reflectEffect("P",x.x)



  import MACROS._

  abstract class Type[T] {
    def fromExp(x: Exp): T
    def toExp(x: T): Exp
  }

  implicit object INT_Type extends Type[INT] {
    def fromExp(x: Exp): INT = INT(x)
    def toExp(x: INT): Exp = x.x
  }

  implicit object SEQ_Type extends Type[SEQ] {
    def fromExp(x: Exp): SEQ = SEQ(x)
    def toExp(x: SEQ): Exp = x.x
  }

  implicit object STRING_Type extends Type[STRING] {
    def fromExp(x: Exp): STRING = STRING(x)
    def toExp(x: STRING): Exp = x.x
  }

  implicit object FUNII_Type extends Type[INT => INT] {
    def fromExp(x: Exp): INT => INT = (y:INT) => INT(g.reflectEffect("@", x, y.x))
    def toExp(x: INT => INT): Exp = g.reflect("λ",g.reify(xn => x(INT(xn)).x))
  }
  implicit object FUNIU_Type extends Type[INT => Unit] {
    def fromExp(x: Exp): INT => Unit = (y:INT) => g.reflectEffect("@", x, y.x)
    def toExp(x: INT => Unit): Exp = g.reflect("λ",g.reify{xn => x(INT(xn)); Const(())})
  }
  implicit object FUNSI_Type extends Type[SEQ => INT] {
    def fromExp(x: Exp): SEQ => INT = (y:SEQ) => INT(g.reflectEffect("@", x, y.x))
    def toExp(x: SEQ => INT): Exp = g.reflect("λ",g.reify(xn => x(SEQ(xn)).x))
  }
  implicit object FUNSU_Type extends Type[SEQ => Unit] {
    def fromExp(x: Exp): SEQ => Unit = (y:SEQ) => g.reflectEffect("@", x, y.x)
    def toExp(x: SEQ => Unit): Exp = g.reflect("λ",g.reify{xn => x(SEQ(xn)); Const(())})
  }

  implicit object TENSOR_Type extends Type[Tensor] {
    def fromExp(x: Exp): Tensor = Tensor(x)
    def toExp(x: Tensor): Exp = x.x
  }


  def reflect[T:Type](s: String, xs: Exp*): T = unref[T](g.reflect(s, xs:_*))
  def reflectEffect[T:Type](s: String, xs: Exp*): T = unref[T](g.reflectEffect(s, xs:_*))
  def ref[T:Type](x: T): Exp = implicitly[Type[T]].toExp(x)
  def unref[T:Type](x: Exp): T = implicitly[Type[T]].fromExp(x)
  object Reflect {
    def unapply(x: Any): Option[(String, List[Exp])] = x match {
      case (s: String, rhs: List[_]) => 
        Some((s,rhs.filter(_.isInstanceOf[Exp]).map(_.asInstanceOf[Exp])))
      case s @ Sym(_) =>
        g.globalDefs.find(_.n == s).map(n => (n.op,n.rhs.filter(_.isInstanceOf[Exp]).map(_.asInstanceOf[Exp])))
      case x: Product if x.productArity == 1 => // INT(s), STRING(s), etc
        unapply(x.productElement(0))
      case _ => None
    }
  }

  var name: String = ""

  var rewrites: (String => Any => Option[Exp]) = (s => x => None)

  def rewriteAt[T:Type](key: Any)(f: PartialFunction[Any,T]) = {
    val old = rewrites
    val f1 = ((s: String) => (x:Any) => if (s == key && f.isDefinedAt(x)) Some(ref(f(x))) else None)
    rewrites = (s => x => f1(s)(x).orElse(old(s)(x)))
  }

  def rewrite[T:Type](f: PartialFunction[Any,T]) = {
    val old = rewrites
    val f1 = ((x:Any) => if (f.isDefinedAt(x)) Some(ref(f(x))) else None)
    rewrites = (s => x => f1(x).orElse(old(s)(x)))
  }

  def rewrite0(f: PartialFunction[Any,Exp]) = {
    val old = rewrites
    val f1 = ((x:Any) => if (f.isDefinedAt(x)) Some(f(x)) else None)
    rewrites = (s => x => f1(x).orElse(old(s)(x)))
  }


  @ir("MultiLoopBuilderLowering") 
  def foobar(arg: INT): INT = 2 * arg

  rewrite { case Mfoobar(Mfoobar(x)) => foobar(x) };


  case class Tensor1(x: Exp)

  case class TensorBuilder1(x: Exp)

  implicit object TENSOR1_Type extends Type[Tensor1] {
    def fromExp(x: Exp): Tensor1 = Tensor1(x)
    def toExp(x: Tensor1): Exp = x.x
  }

  implicit object UNIT_Type extends Type[Unit] {
    def fromExp(x: Exp): Unit = ()
    def toExp(x: Unit): Exp = Const(())
  }

  implicit object ARRAY_Type extends Type[ARRAY] {
    def fromExp(x: Exp): ARRAY = ARRAY(x)
    def toExp(x: ARRAY): Exp = x.x
  }

  implicit object TENSORBUILDER1_Type extends Type[TensorBuilder1] {
    def fromExp(x: Exp): TensorBuilder1 = TensorBuilder1(x)
    def toExp(x: TensorBuilder1): Exp = x.x
  }

  class write extends scala.annotation.StaticAnnotation

  def PRINT(x: Tensor1): Unit =
    g.reflectEffect("P",x.x)


  //@ir("TensorLowering") 
  def tensor_add(a: Tensor1, b: Tensor1): Tensor1 = {
    Tensor1(tensor_shape(a), i => tensor_apply(a,i) + tensor_apply(b,i)) // XXX todo: same shape
  }

  @ir("TensorLowering") 
  def tensor_apply(a: Tensor1, b: SEQ): INT



  @ir("MultiLoopBuilderLowering") 
  def Tensor1(shape: SEQ, f: SEQ=>INT): Tensor1 = {
    val builder = TensorBuilder1(shape)
    forloops(shape, i => builder_add(builder,i,f(i)))
    builder_res(builder)
  }


  @ir def tensor_shape(a: Tensor1): SEQ
  @ir def tensor_builder_shape(a: TensorBuilder1): SEQ

  rewrite { case Mtensor_shape(MTensor1(sh,f)) => sh }
  rewrite { case Mtensor_builder_shape(MTensorBuilder1(sh)) => sh }


  @ir("MultiDimForeachLowering")
  def linearize(sh: SEQ, xs: SEQ): INT

  rewrite { case Mlinearize(SEQ(Const(xs:List[Int])), ys) => 
    linearize1(xs.map(i => INT(Const(i))), ys.explode(xs.length))
  }


  // V-Fusion
  rewriteAt("TensorFusionV") { case Mtensor_apply(MTensor1(shape, f), i) => 
    f(i)
  }


  def linearize1(sh: List[INT], xs: List[INT]): INT = (sh,xs) match {
    case (s::Nil, x::Nil) => x
    case (s::sh, x::xs) => x * sh.reduce(_*_) + linearize1(sh,xs)
  }

  @ir("MultiDimForeachLowering") 
  def product(sh: SEQ): INT = sh match {
    case SEQ(Const(xs: List[Int])) => xs.reduce(_*_)
  }


  @ir
  def TensorBuilderWrap(shape: SEQ, data: ARRAY): TensorBuilder1


  @ir("MultiDimForeachLowering") 
  def TensorBuilder1(@write shape: SEQ): TensorBuilder1 = { // TODO: mark effect differently!
    val data = ARRAY(product(shape))
    TensorBuilderWrap(shape,data)
  }

  @ir("MultiDimForeachLowering") 
  def builder_add(@write builder: TensorBuilder1, i: SEQ, x: INT): Unit = {
    val MTensorBuilderWrap(shape,data) = builder
    data(linearize(shape, i)) = x
  }

  @ir("MultiDimForeachLowering") 
  def builder_res(@write builder: TensorBuilder1): Tensor1 = {
    val MTensorBuilderWrap(shape,data) = builder
    Tensor1(data.x)
  }


  @ir("MultiDimForeachLowering")
  def forloops(@write sz: SEQ, f: SEQ => Unit): Unit = sz match { // TODO: effect poly
    case SEQ(Const(xs: List[Int])) =>
      def forloops1(sz: List[INT])(f: List[INT] => Unit): Unit = sz match {
        case Nil => f(Nil)
        case s::sz => forloop(s, { i => forloops1(sz) { is => f(i::is) } })
      }
      forloops1(xs.map(i => INT(Const(i)))) { i => f(SEQ(i:_*)) }
  }

  @ir("MultiDimForeachLowering")
  def forloop(@write sz: INT, f: INT => Unit): Unit /*= { // TODO: effect poly
    val loopVar = VAR(0)
    WHILE (loopVar() !== sz) { 
      f(loopVar())
      loopVar() = loopVar() + 1
    }
  }*/


  rewrite0 { 
    // NOTE: this will inline ALL function calls! (TODO: should have a more specific mechanism)
    case Reflect("@", List(f@Reflect("λ", Nil), x)) =>
      val Node(s,"λ",List(b@Block(List(in,ein),out,eout))) = g.globalDefs.find(_.n == f).get

      val bound = new Bound
      bound(Graph(g.globalDefs.toList,b))

      val subst = new mutable.HashMap[Def,Exp]
      subst(in) = x
      subst(ein) = g.curBlock

      val nodes = g.globalDefs.filter(n => bound.hm.getOrElse(n.n,Set())(in) || bound.hm.getOrElse(n.n,Set())(ein))
      // println(s"nodes dependent on $in,$ein:")
      // nodes.foreach(println)

      nodes.foreach { case Node(n,op,rhs) => 
        val (effects,pure) = rhs.partition(_.isInstanceOf[Eff])
        val args = pure.map(a => subst.getOrElse(a,a)) // XXX TODO: Blocks!
        if (effects.nonEmpty)
          subst(n) = g.reflectEffect(op,args:_*)
        else
          subst(n) = g.reflect(op,args:_*)
      }

      subst.getOrElse(out,out)
  }

  // NEXT TODO: H-fusion

  // NEXT TODO: fine-grained effect deps

  // NEXT TODO: types, source context, error reporting




  //lower { case `Mfoobar`(arg) => foobar_next(arg) }

  override def mkGraphBuilder() = new MyGraphBuilder()

  class MyGraphBuilder extends GraphBuilder {

    object Def {
      def unapply(xs: Def): Option[(String,List[Def])] = xs match {
        case s @ Sym(n) =>
          globalDefs.find(_.n == s).map(n => (n.op,n.rhs))
        case _ => None
      }
    }

    // registerRewrite (x => (x + x) ====> (2 * x))

    // List("====>", ("+", x, x), ("*", 2, x))
    // List("====>", ("+", x, x), ("*", 2, x))

    override def reflect(s: String, as: Def*): Exp = (s,as.toList) match {
      case ("+", List(Const(a:Int),Const(b:Int))) => Const(a+b)
      case ("-", List(Const(a:Int),Const(b:Int))) => Const(a-b)
      case ("*", List(Const(a:Int),Const(b:Int))) => Const(a*b)
      case ("/", List(Const(a:Int),Const(b:Int))) => Const(a/b)
      case ("%", List(Const(a:Int),Const(b:Int))) => Const(a%b)
      case ("seq", args) if args.forall(_.isInstanceOf[Const]) => Const(args map { case Const(x) => x})
      case ("seq_apply", List(Const(a:Seq[_]),Const(b:Int))) => Const(a(b))
      case ("seq_apply", List(Def("seq", a),Const(b:Int))) => a(b).asInstanceOf[Exp]
      case ("seq_length", List(Const(a:Seq[_]))) => Const(a.length)
      case ("seq_length", List(Def("seq", a),Const(b:Int))) => Const(a.length)
      case ("tensor_shape", List(Def("tensor",List(sh:Exp,f)))) => sh
      case ("tensor_same_shape", List(Const(as), Const(bs))) if as == bs => Const(as)

      //case Mfoobar(x) => ref { 4 * x }

      case p =>
        rewrites(name)(p) match { 
          case Some(e) => e 
          case None => 

        // CSE
        globalDefs.find(n => n.op == s && n.rhs == as).map(_.n)
          .getOrElse(super.reflect(s, as:_*))
        }
    }

    override def reflectEffect(s: String, as: Def*): Exp = (s,as.toList) match {
      case p =>
        rewrites(name)(p) match { 
          case Some(e) => e 
          case None => 
              super.reflectEffect(s, as:_*)
        }

        // CSE?
    }


  }


}




abstract class TensorLowering extends Transformer {
  val frontEnd: TensorFrontEnd
  import frontEnd._
  def init() = {
    frontEnd.name = "TensorLowering"
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
  }

  override def transform(n: Node): Exp = n match {
    case Node(s, "tensor_zeros", List(sh:Exp)) => Tensor(SEQ(transform(sh)))(is => 0).x
    case Node(s, "tensor_ones",  List(sh:Exp)) => Tensor(SEQ(transform(sh)))(is => 1).x
    case Node(s, "tensor_add",  List(a:Exp,b:Exp)) => 
      val (a1,b1) = (Tensor(transform(a)), Tensor(transform(b)))
      val shape = same_shape(a1.shape, b1.shape)
      Tensor(shape)(is => a1(is) + b1(is)).x
    case _ => super.transform(n)
  }
}


abstract class TensorFusionV extends Transformer {
  val frontEnd: TensorFrontEnd
  import frontEnd._
  def init() = {
    frontEnd.name = "TensorFusionV"
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
  }

  val tensors = new mutable.HashMap[Sym, Node]

  // NOTE: fuse _all_ tensors expressions vertically
  // (produce/consume) if possible. This is quite likely overzealous.
  override def transform(n: Node): Exp = n match {
    case Node(s, "tensor", List(sh:Exp, f:Block)) => 
      tensors(s) = n
      super.transform(n)
    case Node(s, "tensor_apply", List(a:Sym,b:Exp)) if tensors contains a => 
      val Node(_, _, List(sh:Exp, f @ Block(arg::block::Nil, res, eff))) = tensors(a)
      if (subst contains arg) println(s"Warning: already have a subst for $arg")
      try { 
        subst(arg) = transform(b)
        traverse(f)
        transform(res)
      } finally subst -= arg 
    case _ => super.transform(n)
  }
}

abstract class TensorFusionH extends Transformer {
  val frontEnd: TensorFrontEnd
  import frontEnd._
  def init() = {
    frontEnd.name = "TensorFusionH"
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
  }

  val tensors = new mutable.HashMap[Sym, Node]

  // NOTE: fuse loops horizontally within a local scope.
  // highly highly preliminary!
  override def traverse(ns: Seq[Node], y: Block): Unit = {
    val loops = ns.filter(n => n.op == "tensor" || n.op == "sum").toList

    val crossdep = new mutable.HashMap[Sym,Set[Sym]] // cross-dep: TODO!!

    if (loops.nonEmpty) {
      // TODO: right now, we assume:
      // - XXX no cross-deps!
      // - XXX all same size!

      val shape = transform(loops.head.rhs.head.asInstanceOf[Exp]) // FIXME!!!
      // for (n @ Node(s,op,args) <- loops) {
      //   println(n + " // " + bound.hm(s))
      // }
      // println("stms:")
      // ns.foreach(println)
      // println("bound:")
      // bound.hm.foreach(println)

      val g = new Graph(inner++ns, y)
      val reach = new mutable.HashSet[Sym]

      val loopSyms = loops.map(_.n).toSet
      val hereSyms = ns.map(_.n).toSet
      var loopsReached = false

      // XXX FIXME: there are some bad performance issues here (g.nodes.find) 
      // and the traversal algorithm is also too simple (need actual top-sort)
      def visit(s: Exp): Unit = s match {
        case s: Sym if !loopSyms(s) && !reach(s) => 
          // we reached a normal statement. mark it,
          // process children, then emit transformed statement
          reach += s
          g.nodes.find(_.n == s).foreach { n => 
            syms(n).foreach(visit)
            if (hereSyms(n.n))
              traverse(n)
          }
        case s: Sym if loopSyms(s) && !loopsReached => 
          // we reached the fused loops
          loopsReached = true
          for (s <- loopSyms) {
            g.nodes.find(_.n == s).foreach { n => 
              syms(n).foreach(visit)
            }
          }
          // emit the fused body ... 
          val loopResPos = new mutable.HashMap[Sym,Int] // local cse on loop results
          val buf = new mutable.ListBuffer[(String,Exp)]
          val newBody = this.g.reify { e => 
            for ((Node(s,op,List(sh:Exp,b@Block(arg::block::Nil, res, eff))),i) <- loops.zipWithIndex) yield {
              assert(transform(sh) == shape, "ERROR: fused loop shapes don't match (TODO!)")
              subst(arg) = e
              traverse(b)
              val r = transform(res)
              val i = buf.indexOf((op,r))
              if (i < 0) {
                loopResPos(s) = buf.length
                buf += ((op,r))
              } else loopResPos(s) = i
            }
            this.g.reflect("seq", buf.map(_._2).toList:_*)
          }
          val fusedLoopSym = this.g.reflect("multiloop", shape, Const(buf.map(_._1).toList), newBody)
          // and now the individual loop bodies
          for ((Node(s,_,_),i) <- loops.zipWithIndex) {
            subst(s) = this.g.reflect("seq_apply", fusedLoopSym, Const(loopResPos(s)))
          }
        case _ =>
      }

      visit(g.block.res)
      visit(g.block.eff)

      // println("---")
      // order.foreach(println)
      // println("---")

    } else {
      super.traverse(ns,y)
    }
  }


  override def transform(n: Node): Exp = n match {
    case Node(s, "tensor", List(sh:Exp, f:Block)) => 
      tensors(s) = n
      super.transform(n)
    case Node(s, "tensor_apply", List(a:Sym,b:Exp)) if tensors contains a => 
      val Node(_, _, List(sh:Exp, f @ Block(arg::block::Nil, res, eff))) = tensors(a)
      subst(arg) = b
      traverse(f)
      transform(res)
    case _ => super.transform(n)
  }
}


abstract class MultiLoopLowering extends Transformer {
  val frontEnd: TensorFrontEnd
  import frontEnd._
  def init() = {
    frontEnd.name = "MultiLoopLowering"
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
  }

  override def transform(n: Node): Exp = n match {
    case Node(s, "multiloop", List(sh: Exp, Const(ops: List[String]), f:Block)) => 
      val INT(Const(dims:Int)) = SEQ(sh).length // TODO: proper error message
      val shape = SEQ(sh).explode(dims)

      def linearize(sh: List[INT], xs: List[INT]): INT = (sh,xs) match {
        case (s::Nil, x::Nil) => x
        case (s::sh, x::xs) => x * sh.reduce(_*_) + linearize(sh,xs)
      }

      class Builder(op: String) { // TODO: proper subclasses?
        val state = if (op == "tensor")
          g.reflectEffect("array_new", shape.reduce(_*_).x)
        else
          g.reflectEffect("var_new", Const(0))

        def +=(i: List[INT], x: INT) = if (op == "tensor")
          g.reflectEffect("array_set", state, linearize(shape,i).x, x.x)
        else
          g.reflectEffect("var_set", state, g.reflect("+", g.reflectEffect("var_get", state), x.x))

        def result() = if (op == "tensor")
          state
        else
          g.reflectEffect("var_get", state)
      }

      val builders = ops map (new Builder(_))

      def forloop(sz: INT)(f: INT => Unit) = {
        val loopVar = VAR(0)
        WHILE (loopVar() !== sz) { 
          f(loopVar())
          loopVar() = loopVar() + 1
        }
      }

      def forloops(sz: List[INT])(f: List[INT] => Unit): Unit = sz match {
        case Nil => f(Nil)
        case s::sz => forloop(s) { i => forloops(sz) { is => f(i::is) } }
      }

      forloops(shape) { e => 
        subst(f.in.head) = SEQ(e:_*).x
        traverse(f)
        val resTuple = transform(f.res)
        for ((b,i) <- builders.zipWithIndex) {
          val res = g.reflect("seq_apply", resTuple, Const(i))
          b += (e,INT(res))
        }
      }

      val res = builders map (_.result())

      g.reflect("seq", res:_*)

    case Node(s, "multiloop", _) => 
      println("TODO: implement lowering for multiloop shape "+n)
      super.transform(n)
    case _ => super.transform(n)
  }
}


abstract class MultiLoopBuilderLowering extends Transformer {
  val frontEnd: TensorFrontEnd
  import frontEnd._
  def init() = {
    frontEnd.name = "MultiLoopBuilderLowering"
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
  }

  override def transform(n: Node): Exp = n match {
    case Node(s, "multiloop", List(sh: Exp, Const(ops: List[String]), f:Block)) => 
      class Builder(op: String) { // TODO: proper subclasses?
        val state = g.reflectEffect(op+"_builder_new", sh)
        def +=(i: SEQ, x: INT) = g.reflectEffect(op+"_builder_add", state, i.x, x.x)
        def result() = g.reflectEffect(op+"_builder_get", state)
      }

      val builders = ops map (new Builder(_))

      def foreach(sz: SEQ)(f: SEQ => Unit): Unit = 
        g.reflectEffect("foreach", sh, g.reify{ e => f(SEQ(e)); Const() })

      foreach(SEQ(sh)) { e => 
        subst(f.in.head) = e.x
        traverse(f)
        val resTuple = transform(f.res)
        for ((b,i) <- builders.zipWithIndex) {
          val res = g.reflect("seq_apply", resTuple, Const(i))
          b += (e,INT(res))
        }
      }

      val res = builders map (_.result())
      g.reflect("seq", res:_*)

    case Node(s, "multiloop", _) => 
      println("TODO: implement lowering for multiloop shape "+n)
      super.transform(n)
    case _ => super.transform(n)
  }
}

abstract class MultiDimForeachLowering extends Transformer {
  val frontEnd: TensorFrontEnd
  import frontEnd._
  def init() = {
    frontEnd.name = "MultiDimForeachLowering"
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
  }

  var builders = new mutable.HashMap[Sym,Node]

  override def transform(n: Node): Exp = n match {

    case Node(s, "foreach", List(sh: Exp, f:Block, eff)) => 

      val INT(Const(dims:Int)) = SEQ(sh).length // TODO: proper error message
      val shape = SEQ(sh).explode(dims)

      def foreach(sz: INT)(f: INT => Unit): Unit = 
        g.reflectEffect("foreach", sz.x, g.reify{ e => f(INT(e)); Const() })

      def forloops(sz: List[INT])(f: List[INT] => Unit): Unit = sz match {
        case Nil => f(Nil)
        case s::sz => foreach(s) { i => forloops(sz) { is => f(i::is) } }
      }

      forloops(shape) { e => 
        subst(f.in.head) = SEQ(e:_*).x
        traverse(f)
        transform(f.res)
      }

      Const(())

    case Node(s, "foreach", _) => 
      println("TODO: implement lowering for multiloop shape "+n)
      super.transform(n)


    case Node(s, "tensor_builder_new", List(sh: Exp, eff)) => 
      builders(s) = n
      val INT(Const(dims:Int)) = SEQ(sh).length // TODO: proper error message
      val shape = SEQ(sh).explode(dims)
      g.reflectEffect("arraype_new", shape.reduce(_*_).x)

    case Node(s, "tensor_builder_add", List(builder: Sym, i: Exp, x: Exp, eff)) => 
      val (Node(s, "tensor_builder_new", List(sh0: Exp, eff))) = builders(builder)
      val sh = transform(sh0) // TODO: tensor_builder_shape
      val INT(Const(dims:Int)) = SEQ(sh).length // TODO: proper error message
      val shape = SEQ(sh).explode(dims)
      val idx = SEQ(transform(i)).explode(dims)
      def linearize(sh: List[INT], xs: List[INT]): INT = (sh,xs) match {
        case (s::Nil, x::Nil) => x
        case (s::sh, x::xs) => x * sh.reduce(_*_) + linearize(sh,xs)
      }
      g.reflectEffect("array_set", transform(builder), linearize(shape,idx).x, x)

    case Node(s, "tensor_builder_get", List(builder: Sym, eff)) => 
      val (Node(s, "tensor_builder_new", List(sh: Exp, eff))) = builders(builder)
      transform(builder)


    case _ => super.transform(n)
  }
}


class TensorTest extends TutorialFunSuite {
  val under = "tensors-"

  val fe = new TensorFrontEnd
  import fe._

  val sc = new internal.ScalaCompile {}
  sc.dumpGeneratedCode = true

  def mkClassName(name: String) = {
    // mangle class name
    (under + name).replace("-","_")
  }

  def testBE(name: String, verbose: Boolean = false)(prog: INT => INT) = {
    test(name) {
      checkOut(name, "scala", {
        var g = program(prog)

        if (verbose) {
          println("// Raw:")
          g.nodes.foreach(println)

          println("// Generic Codegen:")
          (new CodeGen)(g)

          println("// Scala Codegen:")
          (new ScalaCodeGen)(g)

          println("// Compact Scala Codegen:")
          (new CompactScalaCodeGen)(g)
        }

        def emitSource() = {
          val cg = new CompactScalaCodeGen
          if (!verbose) cg.doRename = true

          val arg = cg.quote(g.block.in.head)
          var src = utils.captureOut(cg(g))

          // remove "()" on a single line
          src = src.replaceAll(" *\\(\\) *","") // replac

          // remove unused val x1 = ...
          val names = cg.rename.map(p => p._2).toSet
          for (n <- names) {
            val removed = src.replace(s"val $n = ","")
            if (removed.indexOf(n) < 0)
              src = removed
          }

          val className = mkClassName(name)
          s"def ${className}($arg: Int): Int = {\n $src\n }"        
        }

        println("// Initial code:")
        println(emitSource())


        // lower zeros, ones, etc to uniform tensor constructor
        g = (new TensorLowering { val frontEnd = new TensorFrontEnd; init() }).transform(g)

        println("// After Tensor lowering:")
        println(emitSource())

        // fuse tensor constructors
        g = (new TensorFusionV { val frontEnd = new TensorFrontEnd; init() }).transform(g)

        println("// After Tensor fusion V:")
        println(emitSource())

        g = (new TensorFusionH { val frontEnd = new TensorFrontEnd; init() }).transform(g)

        println("// After Tensor fusion H:")
        println(emitSource())

        val g_fused = g

        g = (new MultiLoopLowering { val frontEnd = new TensorFrontEnd; init() }).transform(g_fused)

        println("// After Multiloop lowering:")
        println(emitSource())


        g = (new MultiLoopBuilderLowering { val frontEnd = new TensorFrontEnd; init() }).transform(g_fused)

        println("// After Multiloop/Builder lowering:")
        println(emitSource())

        g = (new MultiDimForeachLowering { val frontEnd = new TensorFrontEnd; init() }).transform(g)

        println("// After MultiDim foreach lowering:")
        println(emitSource())


        // val cg = new CompactScalaCodeGen
        // cg.doRename = true

        // val arg = cg.quote(g.block.in.head)
        // val src = utils.captureOut(cg(g))
        // sc.dumpGeneratedCode = true

        // val className = mkClassName(name)

        // val fc = sc.compile[Int,Int](className, {
        //   s"// Generated code\nclass ${className} extends (Int => Int) {\n def apply($arg: Int): Int = {\n $src\n }\n }"
        // })

        // println("// Output:")

        // println(fc(0))
        // println(fc(1))
        // println(fc(2))
        // println(fc(3))
        // println(fc(4))
      })
    }
  }

  // basic fusion tests

  testBE("01") { x =>

    val x = ones(SEQ(3,4,5))
    val y = ones(SEQ(3,4,5))
    val z = ones(SEQ(3,4,5))
    val a = x + y
    PRINT(a)
    val b = x + y + z
    PRINT(b)
    0
  }

  testBE("02") { x =>

    val constant = Tensor(100) { i => 1 }
    val linear   = Tensor(100) { i => 2*i(0) }
    val affine   = Tensor(100) { i => constant(i) + linear(i) }
    
    def square(x: INT)      = x*x
    def mean(x: Tensor)     = Sum(x.shape) { i => x(i) } / x.shape(0)
    def variance(x: Tensor) = Sum(x.shape) { i => square(x(i)) } / x.shape(0) - square(mean(x))
    
    val data = affine
    
    val m = mean(data)
    val v = variance(data)

    PRINT(m)
    PRINT(v)
    0
  }

  testBE("03") { x =>

    val m = foobar(7)
    val n = foobar(m)

    PRINT(n)
    0
  }


  testBE("04") { x =>

    val m = Tensor1(SEQ(3,4,5), x => x(0) + x(1) + x(2))
    val n = tensor_add(m,m)
    PRINT(n)
    0
  }


}
