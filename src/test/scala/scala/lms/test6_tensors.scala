package scala.lms

import scala.annotation.implicitNotFound

import scala.collection.{mutable,immutable}
import Backend._

class TensorFrontEnd extends FrontEnd {

  case class SEQ(x: Exp) {
    def apply(y: INT): INT = INT(g.reflect("seq_apply",x,y.x))
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

  override def mkGraphBuilder() = new GraphBuilder {

    object Def {
      def unapply(xs: Def): Option[(String,List[Def])] = xs match {
        case s @ Sym(n) =>
          globalDefs.find(_.n == s).map(n => (n.op,n.rhs))
        case _ => None
      }
    }

    override def reflect(s: String, as: Def*): Exp = (s,as.toList) match {
      case ("+", List(Const(a:Int),Const(b:Int))) => Const(a+b)
      case ("-", List(Const(a:Int),Const(b:Int))) => Const(a-b)
      case ("*", List(Const(a:Int),Const(b:Int))) => Const(a*b)
      case ("/", List(Const(a:Int),Const(b:Int))) => Const(a/b)
      case ("%", List(Const(a:Int),Const(b:Int))) => Const(a%b)
      case ("seq", args) if args.forall(_.isInstanceOf[Const]) => Const(args map { case Const(x) => x})
      case ("seq_apply", List(Const(a:Seq[_]),Const(b:Int))) => Const(a(b))
      case ("tensor_shape", List(Def("tensor",List(sh:Exp,f)))) => sh
      case ("tensor_same_shape", List(Const(as), Const(bs))) if as == bs => Const(as)
      case _ =>
        // CSE
        globalDefs.find(n => n.op == s && n.rhs == as).map(_.n)
          .getOrElse(super.reflect(s, as:_*))
    }
  }


}




abstract class TensorLowering extends Transformer {
  val frontEnd: TensorFrontEnd
  import frontEnd._
  def init() = {
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
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
  }

  val tensors = new mutable.HashMap[Sym, Node]

  // NOTE: fuse loops horizontally within a local scope.
  // highly highly preliminary!
  override def traverse(ns: Seq[Node], y: Block): Unit = {
    val loops = ns.filter(_.op == "tensor")

    val hm = new mutable.HashMap[Sym,Set[Sym]] // cross-dep: TODO!!

    if (loops.nonEmpty) {
      // TODO: right now, we assume:
      // - XXX no cross-deps!
      // - XXX all same size!

      val shape = loops.head.rhs.head // FIXME!!!
      // for (n @ Node(s,op,args) <- loops) {
      //   println(n + " // " + bound.hm(s))
      // }
      // println("stms:")
      // ns.foreach(println)
      // println("bound:")
      // bound.hm.foreach(println)

      // now how to do the actual fusion?? need to re-sort !!!

      val g = new Graph(inner++ns, y)
      val reach = new mutable.HashSet[Sym]
      //val order = new mutable.ArrayBuffer[Node]

      val loopSyms = loops.map(_.n).toSet
      val hereSyms = ns.map(_.n).toSet
      var loopsReached = false

     // XXX FIXME: there are some bad performance issues here (g.nodes.find) ....
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
          val newBody = this.g.reify { e => 
            val res = for ((Node(s,_,List(sh,b@Block(arg::block::Nil, res, eff))),i) <- loops.zipWithIndex) yield {
              assert(sh == shape, "fused loop shapes don't match (TODO!)")
              subst(arg) = e
              traverse(b)
              transform(res)
            }
            this.g.reflect("seq", res:_*)
          }
          val fusedLoopSym = this.g.reflect("fused-tensor", shape, newBody)
          // and now the individual loop bodies
          for ((Node(s,_,_),i) <- loops.zipWithIndex) {
            subst(s) = this.g.reflect("seq-apply", fusedLoopSym, Const(i))
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
          //cg.doRename = true

          val arg = cg.quote(g.block.in.head)
          val src = utils.captureOut(cg(g))
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

    val linear = Tensor(100) { i => 2*i(0) }

    val affine = Tensor(100) { i => constant(i) + linear(i) }
    
    def square(x: INT) = x*x
    def mean(x: Tensor) = Sum(x.shape) { i => x(i) } / x.shape(0)
    def variance(x: Tensor) = Sum(x.shape) { i => square(x(i)) } / x.shape(0) - square(mean(x))
    
    val data = affine
    
    val m = mean(data)
    val v = variance(data)

    PRINT(m)
    PRINT(v)
    0
  }

}
