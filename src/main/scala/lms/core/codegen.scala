package lms.core

import lms.macros.RefinedManifest

import scala.collection.mutable

import java.io.{ByteArrayOutputStream, PrintStream}

import Backend._

class Unknown // HACK: Sentinel for typeMap

class GenericCodeGen extends Traverser {

  def emit(s: String) = println(s)

  override def traverse(ns: Seq[Node], y: Block): Unit = {
    emit(s"// in: ${y.in.mkString(" ")} effect: ${y.ein}") // XXX compat
    super.traverse(ns, y)
    emit(y.res.toString + " // out effect: " + y.eff.toString)
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f, "λ", List(y: Block), _) =>
      emit(s"$f = (λ {")
      traverse(y, f)
      emit(s"})")
    case n @ Node(f, op, es, eff) =>
      val ss = es map {
        case e @ Block(_,_,_,_) => "{" + utils.captureOut(traverse(e)) + "}" // FIXME freq!!
        case e => e.toString
      }
      val efc = if (eff.deps.nonEmpty) s" // Eff: ${eff}" else ""
      emit(s"$f = ($op ${ss.mkString(" ")})$efc")
  }
}

/*
 * Resolve may dependencies
 */
class DeadCodeElimCG {

  final val fixDeps = true // remove deps on removed nodes
  var live: collection.Set[Sym] = _
  var reach: collection.Set[Sym] = _

  def valueSyms(n: Node): List[Sym] =
    directSyms(n) ++ blocks(n).flatMap {
      case Block(ins, res:Sym, ein, _) => res::ein::ins
      case Block(ins, _, ein, _) => ein::ins
    }

  // staticData -- not really a DCE task, but hey
  var statics: collection.Set[Node] = _

  def apply(g: Graph): Graph = utils.time("DeadCodeElimCG") {

    live = new mutable.HashSet[Sym]
    reach = new mutable.HashSet[Sym]
    statics = new mutable.HashSet[Node]
    var newNodes: List[Node] = Nil
    val used = new mutable.HashSet[Exp]
    var size = 0

    // First pass liveness and reachability
    // Only a single pass that reduce input size and first step of the next loop
    utils.time("A_First_Path") {
      reach ++= g.block.used
      if (g.block.res.isInstanceOf[Sym]) {
        live += g.block.res.asInstanceOf[Sym]
        used += g.block.res.asInstanceOf[Sym]
      }
      used ++= g.block.bound
      for (d <- g.nodes.reverseIterator) {
        if (reach(d.n)) {
          val nn = d match {
            case n @ Node(s, "?", c::(a:Block)::(b:Block)::t, eff) if !live(s) =>
              n.copy(rhs = c::a.copy(res = Const(()))::b.copy(res = Const(()))::t) // remove result deps if dead
            case _ => d
          }
          live ++= valueSyms(nn)
          reach ++= hardSyms(nn)

          // if (!used(nn.n)) {
          //   if (nn.eff.hasSimpleEffect || nn.eff.wkeys.exists(used)) {
          //     used += nn.n
          //     used ++= valueSyms(nn)
          //   }
          // } else {
          //   used ++= valueSyms(nn)
          // }
          newNodes = nn::newNodes
        }
      }
    }

    // Second pass remove unused variables
    var idx: Int = 1
    while (size != used.size) {
      utils.time(s"Extra_Path_$idx") {
        size = used.size
        for (d <- newNodes.reverseIterator) {
          if (!used(d.n)) {
            if (d.eff.hasSimpleEffect || d.eff.wkeys.exists(used)) {
              used += d.n
              used ++= valueSyms(d)
            }
          } else {
            used ++= valueSyms(d)
          }
        }
      }
      idx += 1
    }

    utils.time(s"Recreate_the_graph") {
      var newGlobalDefsCache = Map[Sym,Node]()
      newNodes = for (d <- newNodes if used(d.n)) yield {
        newGlobalDefsCache += d.n -> d
        if (d.op == "staticData") statics += d
        if (fixDeps)
          d.copy(rhs = d.rhs.map {
            case b: Block => b.copy(eff = b.eff.filter(used))
            case o => o
          }, eff = d.eff.filter(used))
        else
          d
      }
      val newBlock = if (fixDeps)
        g.block.copy(eff = g.block.eff.filter(used))
      else
        g.block

      Graph(newNodes, newBlock, newGlobalDefsCache)
    }
  }
}

trait ExtendedCodeGen {

  var typeMap: collection.Map[lms.core.Backend.Exp, Manifest[_]] = _
  def typeBlockRes(x: lms.core.Backend.Exp) = x match {
    case Const(()) => manifest[Unit]
    case Const(x: Int) => manifest[Int]
    case Const(x: Long) => manifest[Long]
    case Const(x: Float) => manifest[Float]
    case Const(x: Double) => manifest[Double]
    case Const(x: Char) => manifest[Char]
    case Const(x: String) => manifest[String]
    case Const(_) => ???
    case _ => typeMap.getOrElse(x, manifest[Unknown])
  }

  val dce = new DeadCodeElimCG

  var stream: java.io.PrintStream = _
  def withStream(out: PrintStream)(f: => Unit) = {
    val save = stream
    stream = out
    f
    stream = save
  }
  def capture(f: => Unit) = {
    val buf = new ByteArrayOutputStream()
    val bufWriter = new PrintStream(buf)
    withStream(bufWriter)(f)
    buf
  }

  def emit(buf: ByteArrayOutputStream): Unit = buf.writeTo(stream)

  def quote(s: Def): String
  def array(innerType: String): String
  def primitive(rawType: String): String
  def record(man: RefinedManifest[_]): String
  def function(sig: List[Manifest[_]]): String
  def remapUnsigned(m: Manifest[_]): String
  def remap(m: Manifest[_]): String = m.typeArguments match {
    case Nil => m match {
      case ref: RefinedManifest[_] => record(ref)
      case _ => primitive(m.toString)
    }
    case List(inner) => array(remap(inner))
    case sig => function(sig)
  }

  def init(g: Graph): Graph = dce(g)

  def quoteStatic(n: Node) = n match {
    case Node(s, "staticData", List(Const(a)), _) =>
      val arg = "p"+quote(s)
      val tpe = a match { // FIXME: hardcoded ...
        case a: Array[Array[Int]] => "Array[Array[Int]]"
        case a: Array[Int] => "Array[Int]"
        case a: Array[Array[Double]] => "Array[Array[Double]]"
        case a: Array[Double] => "Array[Double]"
        case a: Array[Array[Float]] => "Array[Array[Float]]"
        case a: Array[Float] => "Array[Float]"
        case a: Array[Array[Long]] => "Array[Array[Long]]"
        case a: Array[Long] => "Array[Long]"
      }
      s"$arg: $tpe"
  }

  def extractStatic(n: Node) = n match {
    case Node(s, "staticData", List(Const(a)), _) =>
      (a.getClass, a)
  }

  def extractAllStatics() = dce.statics.toList.map(extractStatic)

  def emitAll(g: Graph, name: String)(m1:Manifest[_],m2:Manifest[_]): Unit

  def nameMap: Map[String, String]
}
