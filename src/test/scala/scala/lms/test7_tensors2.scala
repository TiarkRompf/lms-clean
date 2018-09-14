package scala.lms

import scala.annotation.implicitNotFound

import scala.collection.{mutable,immutable}
import Backend._

import util.GraphUtil

class TensorFrontEnd2 extends FrontEnd {

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
    g.reflectEffect("P",x.x)(CTRL)
  def PRINT(x: Tensor): Unit =
    g.reflectEffect("P",x.x)(CTRL)



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

  implicit object UNIT_Type extends Type[Unit] {
    def fromExp(x: Exp): Unit = ()
    def toExp(x: Unit): Exp = Const(())
  }

  implicit object ARRAY_Type extends Type[ARRAY] {
    def fromExp(x: Exp): ARRAY = ARRAY(x)
    def toExp(x: ARRAY): Exp = x.x
  }

  implicit object FUNII_Type extends Type[INT => INT] {
    def fromExp(x: Exp): INT => INT = (y:INT) => INT(g.reflectEffect("@", x, y.x)(CTRL))
    def toExp(x: INT => INT): Exp = g.reflect("λ",g.reify(xn => x(INT(xn)).x))
  }
  implicit object FUNIU_Type extends Type[INT => Unit] {
    def fromExp(x: Exp): INT => Unit = (y:INT) => g.reflectEffect("@", x, y.x)(CTRL)
    def toExp(x: INT => Unit): Exp = g.reflect("λ",g.reify{xn => x(INT(xn)); Const(())})
  }
  implicit object FUNSI_Type extends Type[SEQ => INT] {
    def fromExp(x: Exp): SEQ => INT = (y:SEQ) => INT(g.reflectEffect("@", x, y.x)(CTRL))
    def toExp(x: SEQ => INT): Exp = g.reflect("λ",g.reify(xn => x(SEQ(xn)).x))
  }
  implicit object FUNSU_Type extends Type[SEQ => Unit] {
    def fromExp(x: Exp): SEQ => Unit = (y:SEQ) => g.reflectEffect("@", x, y.x)(CTRL)
    def toExp(x: SEQ => Unit): Exp = g.reflect("λ",g.reify{xn => x(SEQ(xn)); Const(())})
  }


  def reflect[T:Type](s: String, xs: Exp*)(efs: Exp*): T = {
    unref[T](g.reflectEffect(s, xs:_*)(efs:_*))
  }
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


  //var name: String = ""

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

  implicit object TENSORBUILDER1_Type extends Type[TensorBuilder1] {
    def fromExp(x: Exp): TensorBuilder1 = TensorBuilder1(x)
    def toExp(x: TensorBuilder1): Exp = x.x
  }

  class write extends scala.annotation.StaticAnnotation

  def PRINT(x: Tensor1): Unit =
    g.reflectEffect("P",x.x)(CTRL)


  @ir("TensorLowering") 
  def tensor_add1(a: Tensor1, b: Tensor1): Tensor1 = {
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
  def TensorBuilder1(shape: SEQ): TensorBuilder1 @write = {
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
  def forloops(sz: SEQ, f: SEQ => Unit): Unit = sz match {
    case SEQ(Const(xs: List[Int])) =>
      def forloops1(sz: List[INT])(f: List[INT] => Unit): Unit = sz match {
        case Nil => f(Nil)
        case s::sz => forloop(s, { i => forloops1(sz) { is => f(i::is) } })
      }
      forloops1(xs.map(i => INT(Const(i)))) { i => f(SEQ(i:_*)) }
  }

  @ir("MultiDimForeachLowering")
  def forloop(sz: INT, f: INT => Unit): Unit /*= {
    val loopVar = VAR(0)
    WHILE (loopVar() !== sz) { 
      f(loopVar())
      loopVar() = loopVar() + 1
    }
  }*/


  rewrite0 { 
    // NOTE: this will inline ALL function calls! (TODO: should have a more specific mechanism)
    case Reflect("@", List(f@Reflect("λ", Nil), x)) =>
      val Node(s,"λ",List(b@Block(List(in,ein),out,eout)),_) = g.globalDefs.find(_.n == f).get

      val bound = new Bound
      bound(Graph(g.globalDefs.toList,b))

      val subst = new mutable.HashMap[Def,Exp]
      subst(in) = x
      subst(ein) = g.effectToExp(g.curBlock)

      val nodes = g.globalDefs.filter(n => bound.hm.getOrElse(n.n,Set())(in) || bound.hm.getOrElse(n.n,Set())(ein))
      // println(s"nodes dependent on $in,$ein:")
      // nodes.foreach(println)

      nodes.foreach { case Node(n,op,rhs,efs) => 
        val (effects,pure) = rhs.partition(_.isInstanceOf[Eff])
        val args = pure.map(a => subst.getOrElse(a,a)) // XXX TODO: Blocks!
        val efs1 = efs.map(a => subst.getOrElse(a,a))
        // XXX losing effect stuff here !!!
        if (effects.nonEmpty)
          subst(n) = g.reflectEffect(op,args:_*)(efs1:_*)
        else
          subst(n) = g.reflect(op,args:_*)
      }

      subst.getOrElse(out,out)
  }


  override def mkGraphBuilder() = new MyGraphBuilder()

  class MyGraphBuilder extends GraphBuilder {
    rewrites = TensorFrontEnd2.this.rewrites

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

      case p =>
        super.reflect(s, as:_*)
    }
  }


}



class TensorTransformer(name: String) extends Transformer {
  val frontEnd: TensorFrontEnd2 = new TensorFrontEnd2
  frontEnd.g = frontEnd.mkGraphBuilder()
  frontEnd.g.name = name
  g = frontEnd.g
}



class TensorFusionH2 extends TensorTransformer("TensorFusionH2") {
  import frontEnd._

  // NOTE: fuse loops horizontally within a local scope.
  // highly highly preliminary!
  override def traverse(ns: Seq[Node], y: Block): Unit = {
    val loops = ns.filter(n => n.op == "forloops" || n.op == "forloop").toList

    val crossdep = new mutable.HashMap[Sym,Set[Sym]] // cross-dep: TODO!!

    if (loops.nonEmpty) {
      // TODO: right now, we assume:
      // - XXX no cross-deps!
      // - XXX all same size! (fix: group by shape)

      val shape = transform(loops.head.rhs.head.asInstanceOf[Exp]) // FIXME!!!

      // for (n @ Node(s,op,args, _) <- loops) {
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

      //def stronglyConnectedComponents[T](start: List[T], succ: T=>List[T]): List[List[T]]

      def find(s: Exp): List[Node] = s match {
        case s: Sym if loopSyms(s) => loops
        case _ => g.nodes.filter(_.n == s).toList
      }

      def dep(n: Node): List[Exp] = syms(n)

      val scc = GraphUtil.stronglyConnectedComponents[List[Node]](
        find(g.block.res)::g.block.eff.map(find),
        es => es.map(dep).flatMap(_.map(find))
      )

      val scc1 = scc.reverse
      
      //scc1.foreach(println)

      scc1.foreach {
        case List(List(n)) if ns contains n =>
          traverse(n)
        case List(List(n)) =>
          // not at top-level --> ignore
        case List(List()) =>
          // first --> ignore
        case List(xs) =>
          assert(xs == loops, s"$xs != $loops") //XXX TODO error message!

          // emit the fused body ... 
          val newBody = this.g.reify { e => 
            for (l <- loops) {
              val (Node(s,op,List(sh:Exp,f:Exp,_), _)) = l
              assert(transform(sh) == shape, "ERROR: fused loop shapes don't match (TODO!)")
              this.g.reflectEffect("@", transform(f), e)() //XXX effect
            }
            Const(())
          }
          val fusedLoopSym = this.g.reflectEffect("forloops", shape, this.g.reflect("λ",newBody))(this.g.getEfs(newBody):_*)
      }

    } else {
      super.traverse(ns,y)
    }
  }


}




class TensorTest2 extends TutorialFunSuite {
  val under = "tensors-"

  val fe = new TensorFrontEnd2
  import fe._

  val sc = new internal.ScalaCompile {}
  sc.dumpGeneratedCode = true

  def mkClassName(name: String) = {
    // mangle class name
    (under + name).replace("-","_")
  }

  def testBE(name: String, verbose: Boolean = false, alt: Boolean = false, eff: Boolean = false)(prog: INT => INT) = {
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
          if (eff)      cg.doPrintEffects = true

          val arg = cg.quote(g.block.in.head)
          val efs = cg.quoteEff(g.block.in.last)
          var src = utils.captureOut(cg(g))

          if (!verbose) {
            // remove "()" on a single line
            src = src.replaceAll(" *\\(\\) *","")

            // remove unused val x1 = ...
            val names = cg.rename.map(p => p._2).toSet
            for (n <- names) {
              val removed = src.replace(s"val $n = ","")
              if (removed.indexOf(n) < 0)
                src = removed
            }
          }

          val className = mkClassName(name)
          s"def ${className}($arg: Int): Int$efs = {\n $src\n }"
        }

        println("// Initial code:")
        println(emitSource())


        // lower zeros, ones, etc to uniform tensor constructor
        g = (new TensorTransformer("TensorLowering")).transform(g)

        println("// After Tensor lowering:")
        println(emitSource())

        // fuse tensor constructors
        g = (new TensorTransformer("TensorFusionV")).transform(g)

        println("// After Tensor fusion V:")
        println(emitSource())

        g = (new TensorTransformer("TensorFusionH")).transform(g)

        println("// After Tensor fusion H:")
        println(emitSource())

        g = (new TensorTransformer("MultiLoopBuilderLowering")).transform(g)

        println("// After Multiloop/Builder lowering:")
        println(emitSource())

        g = (new TensorFusionH2).transform(g)

        println("// After Tensor fusion H2:")
        println(emitSource())

        g = (new TensorTransformer("MultiDimForeachLowering")).transform(g)

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

/*  testBE("01") { x =>

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
*/
  testBE("03") { x =>

    val m = foobar(7)
    val n = foobar(m)

    PRINT(n)
    0
  }


  testBE("04") { x =>

    val m = Tensor1(SEQ(3,4,5), x => x(0) + x(1) + x(2))
    val n = tensor_add1(m,m)
    PRINT(n)
    0
  }

  testBE("05", alt = true, eff=true) { x =>

    val m = Tensor1(SEQ(3,4,5), x => x(0) + x(1) + x(2))
    val a = tensor_add1(m,m)
    PRINT(a)
    val b = tensor_add1(tensor_add1(m,m),m)
    PRINT(b)
    0
  }

}
