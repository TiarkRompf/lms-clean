package lms
package tensors

import scala.annotation.implicitNotFound

import scala.collection.{mutable,immutable}

import lms.core._
import lms.util.GraphUtil

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
    g.reflectEffect("P",x.x)(x.x)(CTRL) // read on x.x
  def PRINT(x: Tensor): Unit =
    g.reflectEffect("P",x.x)(x.x)(CTRL) // read on x.x


  override def mkGraphBuilder() = new MyGraphBuilder()

  class MyGraphBuilder extends GraphBuilder {

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

      case p =>
        super.reflect(s, as:_*)
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
    case Node(s, "tensor_zeros", List(sh:Exp), _) => Tensor(SEQ(transform(sh)))(is => 0).x
    case Node(s, "tensor_ones",  List(sh:Exp), _) => Tensor(SEQ(transform(sh)))(is => 1).x
    case Node(s, "tensor_add",  List(a:Exp,b:Exp), _) =>
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

  val tensors = new mutable.HashMap[Sym, (Node, List[Sym], Seq[Node])]

  // NOTE: fuse _all_ tensors expressions vertically
  // (produce/consume) if possible. This is quite likely overzealous.
  override def transform(n: Node): Exp = n match {
    case Node(s, "tensor", List(sh:Exp, f:Block), _) =>
      tensors(s) = (n, path, inner)
      super.transform(n)
    case Node(s, "tensor_apply", List(a:Sym,b:Exp), _) if tensors contains a =>
      val (Node(_, _, List(sh:Exp, f @ Block(arg::Nil, res, block, eff)), _), path0, inner0) = tensors(a)
      if (subst contains arg) println(s"Warning: already have a subst for $arg")
      try {
        subst(arg) = transform(b)
        withResetScope(path0, inner0) {
          traverse(f)
        }
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

  val tensors = new mutable.HashMap[Sym, (Node, List[Sym], Seq[Node])]

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
      // for (n @ Node(s,op,args, _) <- loops) {
      //   println(n + " // " + bound.hm(s))
      // }
      // println("stms:")
      // ns.foreach(println)
      // println("bound:")
      // bound.hm.foreach(println)

      val g = new Graph(inner++ns, y, null)
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
            for ((Node(s,op,List(sh:Exp,b@Block(arg::Nil, res, block, eff)), _),i) <- loops.zipWithIndex) yield {
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
          for ((Node(s,_,_, _),i) <- loops.zipWithIndex) {
            subst(s) = this.g.reflect("seq_apply", fusedLoopSym, Const(loopResPos(s)))
          }
        case _ =>
      }

      visit(g.block.res)
      g.block.eff.deps.foreach(visit)

      // println("---")
      // order.foreach(println)
      // println("---")

    } else {
      super.traverse(ns,y)
    }
  }


  override def transform(n: Node): Exp = n match {
    case Node(s, "tensor", List(sh:Exp, f:Block), _) =>
      tensors(s) = (n, path, inner)
      super.transform(n)
    case Node(s, "tensor_apply", List(a:Sym,b:Exp), _) if tensors contains a =>
      val (Node(_, _, List(sh:Exp, f @ Block(arg::Nil, res, block, eff)), _), path0, inner0) = tensors(a)
      subst(arg) = b
      withResetScope(path0, inner0) {
        traverse(f)
      }
      transform(res)
    case _ => super.transform(n)
  }
}




abstract class MultiLoopLowering extends Transformer {
  val frontEnd: TensorFrontEnd
  import frontEnd._
  def init() = {
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
  }

  override def transform(n: Node): Exp = n match {
    case Node(s, "multiloop", List(sh: Exp, Const(ops: List[String]), f:Block), _) =>
      val INT(Const(dims:Int)) = SEQ(sh).length // TODO: proper error message
      val shape = SEQ(sh).explode(dims)

      def linearize(sh: List[INT], xs: List[INT]): INT = (sh,xs) match {
        case (s::Nil, x::Nil) => x
        case (s::sh, x::xs) => x * sh.reduce(_*_) + linearize(sh,xs)
      }

      class Builder(op: String) { // TODO: proper subclasses?
        val state = if (op == "tensor")
          g.reflectMutable("array_new", shape.reduce(_*_).x)
        else
          g.reflectMutable("var_new", Const(0))

        def +=(i: List[INT], x: INT) = if (op == "tensor")
          g.reflectWrite("array_set", state, linearize(shape,i).x, x.x)(state)
        else
          g.reflectWrite("var_set", state, g.reflect("+", g.reflectRead("var_get", state)(state), x.x))(state)

        def result() = if (op == "tensor")
          state
        else
          g.reflectRead("var_get", state)(state)
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

    case Node(s, "multiloop", _, _) =>
      println("TODO: implement lowering for multiloop shape "+n)
      super.transform(n)
    case _ => super.transform(n)
  }
}


abstract class MultiLoopBuilderLowering extends Transformer {
  val frontEnd: TensorFrontEnd
  import frontEnd._
  def init() = {
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
  }

  override def transform(n: Node): Exp = n match {
    case Node(s, "multiloop", List(sh: Exp, Const(ops: List[String]), f:Block), _) =>
      class Builder(op: String) { // TODO: proper subclasses?
        val state = g.reflectMutable(op+"_builder_new", sh)
        def +=(i: SEQ, x: INT) = g.reflectWrite(op+"_builder_add", state, i.x, x.x)(state)
        def result() = g.reflectRead(op+"_builder_get", state)(state)
      }

      val builders = ops map (new Builder(_))

      def foreach(sz: SEQ)(f: SEQ => Unit): Unit = {
        val b = g.reify{ e => f(SEQ(e)); Const() }
        val summary = g.getEffKeys(b)
        g.reflectEffectSummary("foreach", sh, b)(summary)
      }

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

    case Node(s, "multiloop", _, _) =>
      println("TODO: implement lowering for multiloop shape "+n)
      super.transform(n)
    case _ => super.transform(n)
  }
}

abstract class MultiDimForeachLowering extends Transformer {
  val frontEnd: TensorFrontEnd
  import frontEnd._
  def init() = {
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
  }

  var builders = new mutable.HashMap[Sym,Node]

  override def transform(n: Node): Exp = n match {

    case Node(s, "foreach", List(sh: Exp, f:Block), _) =>

      val INT(Const(dims:Int)) = SEQ(sh).length // TODO: proper error message
      val shape = SEQ(sh).explode(dims)

      def foreach(sz: INT)(f: INT => Unit): Unit = {
        val b = g.reify{ e => f(INT(e)); Const() }
        val summary = g.getEffKeys(b)
        g.reflectEffectSummary("foreach", sz.x, b)(summary)
      }

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

    case Node(s, "foreach", _, _) =>
      println("TODO: implement lowering for multiloop shape "+n)
      super.transform(n)


    case Node(s, "tensor_builder_new", List(sh: Exp), _) =>
      builders(s) = n
      val INT(Const(dims:Int)) = SEQ(sh).length // TODO: proper error message
      val shape = SEQ(sh).explode(dims)
      g.reflectMutable("array_new", shape.reduce(_*_).x)

    case Node(s, "tensor_builder_add", List(builder: Sym, i: Exp, x: Exp), _) =>
      val (Node(s, "tensor_builder_new", List(sh0: Exp), _)) = builders(builder)
      val sh = transform(sh0) // TODO: tensor_builder_shape
      val INT(Const(dims:Int)) = SEQ(sh).length // TODO: proper error message
      val shape = SEQ(sh).explode(dims)
      val idx = SEQ(transform(i)).explode(dims)
      def linearize(sh: List[INT], xs: List[INT]): INT = (sh,xs) match {
        case (s::Nil, x::Nil) => x
        case (s::sh, x::xs) => x * sh.reduce(_*_) + linearize(sh,xs)
      }
      g.reflectWrite("array_set", transform(builder), linearize(shape,idx).x, x)(transform(builder))

    case Node(s, "tensor_builder_get", List(builder: Sym), _) =>
      val (Node(s, "tensor_builder_new", List(sh: Exp), _)) = builders(builder)
      transform(builder)


    case _ => super.transform(n)
  }
}


class TensorTest extends TutorialFunSuite {
  val under = "demos/tensors/tensors-"

  val fe = new TensorFrontEnd
  import fe._

  val sc = new util.ScalaCompile {}
  sc.dumpGeneratedCode = true

  def mkClassName(name: String) = {
    // mangle class name
    ("tensors-" + name).replace("-","_")
  }

  def testBE(name: String, verbose: Boolean = false, alt: Boolean = false, eff: Boolean = false)(prog: INT => INT) = {
    test(name) {
      checkOut(name, "scala", {
        var g = HardenMayHardDeps(program(prog))

        if (verbose) {
          println("// Raw:")
          g.nodes.foreach(println)

          println("// Generic Codegen:")
          (new GenericCodeGen)(g)

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
          val efs = cg.quoteEff(g.block.ein)
          var src = utils.captureOut(cg(g))

          if (!verbose) {
            // remove unused val x1 = ...
            val names = cg.rename.map(p => p._2).toSet
            for (n <- names) {
              val removed = src.replace(s"val $n = ","")
              if (removed.indexOf(n) < 0)
                src = removed
            }
          }

          val className = mkClassName(name)
          s"def ${className}($arg: Int): Int$efs = $src"
        }

        println("// Initial code:")
        println(emitSource())


        // lower zeros, ones, etc to uniform tensor constructor
        g = HardenMayHardDeps(g) // TMP need handle
        g = (new TensorLowering { val frontEnd = new TensorFrontEnd; init() }).transform(g)

        println("// After Tensor lowering:")
        println(emitSource())

        // fuse tensor constructors
        g = HardenMayHardDeps(g) // TMP need handle
        g = (new TensorFusionV { val frontEnd = new TensorFrontEnd; init() }).transform(g)

        System.out.println("\n\n\n")
        println("// After Tensor fusion V:")
        println(emitSource())

        g = HardenMayHardDeps(g) // TMP need handle
        g = (new TensorFusionH { val frontEnd = new TensorFrontEnd; init() }).transform(g)

        System.out.println("\n\n\n")
        println("// After Tensor fusion H:")
        println(emitSource())

        if (!alt) {
          val g_fused = g

          g = HardenMayHardDeps(g) // TMP need handle
          g = (new MultiLoopLowering { val frontEnd = new TensorFrontEnd; init() }).transform(g_fused)

          System.out.println("\n\n\n")
          println("// After Multiloop lowering:")
          println(emitSource())

          g = g_fused
        }

        g = HardenMayHardDeps(g) // TMP need handle
        g = (new MultiLoopBuilderLowering { val frontEnd = new TensorFrontEnd; init() }).transform(g)

        System.out.println("\n\n\n")
        println("// After Multiloop/Builder lowering:")
        println(emitSource())

        g = HardenMayHardDeps(g) // TMP need handle
        g = (new MultiDimForeachLowering { val frontEnd = new TensorFrontEnd; init() }).transform(g)

        System.out.println("\n\n\n")
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

}
