package lms.core

/*
  LMS compiler back-end in one file.

  Functionality:
  - Graph IR
  - Rewriting & CSE
  - Code motion & DCE
  - Effects (fine-grained, with aliasing)
  - Transformers

  Classic compiler fare (tbd):
  - Parsing
  - Typing
  - CPS
  - Dataflow
  - Closure conversion
  - Register allocation

*/

import scala.collection.mutable

object Backend {

  // Expressions are first-class entities in a program
  // that can be manipulated freely. An expression is
  // either a constant or a symbolic reference.
  abstract class Exp extends Def

  case class Sym(n: Int) extends Exp {
    override def toString = s"x$n"
  }
  case class Const(x: Any) extends Exp

  // A list of effect keys (mutable vars, ...) and
  // dependencies (previous writes, ...)
  //
  // TOOD: this can be made more fine-grained, i.e.,
  // distinguish may/must and read/write effects, 
  // soft dependencies, ...

  case class EffectSummary(deps: List[Exp], keys: List[Exp]) {
    override def toString = if (deps.nonEmpty || keys.nonEmpty)
      s"[${keys.mkString(" ")}: ${deps.mkString(" ")}]" else ""
  }

  // A node in a computation graph links a symbol with
  // its definition, consisting of an operator and its
  // arguments, as well as an effect summary.

  case class Node(n: Sym, op: String, rhs: List[Def], eff: EffectSummary) {
    override def toString = s"$n = ($op ${rhs.mkString(" ")})  $eff"
  }

  /* YYY TODO
    + Node should have a separate field for effect dependencies
      in addition to effect keys. Currently they are lumped
      at the end of rhs (wrapped in Eff(...)) which does not
      seem ideal.
    - Block should separate out input effects, too
    - Should (can?) we get rid of type Def? what prevents
      us from allowing any value as part of rhs
      (guess: typeclass to/fro conversion for FrontEnd)
  */


  // A definition is part of the right-hand side of a 
  // node definition.
  abstract class Def

  // inputs, result, effect input, effect output summary
  case class Block(in: List[Sym], res: Exp, ein: Sym, eff: EffectSummary) extends Def {
    def bound = ein::in
    def used = (res::eff.deps).distinct collect { case s: Sym => s }
  }


  // where should this go?
  def boundSyms(x: Node): Seq[Sym] = blocks(x) flatMap (_.bound)

  def blocks(x: Node): List[Block] = 
    x.rhs.collect { case a @ Block(_,_,_,_) => a }

  def directSyms(x: Node): List[Sym] = 
    x.rhs.flatMap { 
      case s: Sym => List(s) 
      case _ => Nil
    }

  def syms(x: Node): List[Sym] = 
    x.rhs.flatMap {
      case s: Sym => List(s) 
      case b: Block => b.used
      case _ => Nil
    } ++ x.eff.deps.collect { case s: Sym => s }
}

import Backend._


class GraphBuilder {
  val globalDefs = new mutable.ArrayBuffer[Node]

  var nSyms = 0
  def fresh = try nSyms finally nSyms += 1

  object Def {
    def unapply(xs: Def): Option[(String,List[Def])] = xs match {
      case s @ Sym(n) =>
        globalDefs.find(_.n == s).map(n => (n.op,n.rhs))
      case _ => None
    }
  }

  var name: String = ""

  var rewrites: (String => Any => Option[Exp]) = (s => x => None)

  def rewrite(f: PartialFunction[Any,Exp]) = {
    val old = rewrites
    val f1 = ((x:Any) => if (f.isDefinedAt(x)) Some(f(x)) else None)
    rewrites = (s => x => f1(x).orElse(old(s)(x)))
  }


  def reflect(s: String, as: Def*): Exp = {
    reflectEffect(s, as:_*)()
  }

  def reflectEffect(s: String, as: Def*)(efs: Exp*): Exp = {
    // rewrite?
    rewrites(name)((s,as.toList)) match {
      case Some(e) => e 
      case None => 

      // latent effects? closures, mutable vars, ... (these are the keys!)
      val efs2 = (as.toList.flatMap(getLatentEffect) ++ efs).distinct

      // effects or pure?
      if (efs2.nonEmpty) {
        val sm = Sym(fresh) 
        // gather effect dependencies
        val prev = effectForKeys(curBlock,efs2)
        try reflect(sm, s, as:_*)(prev:_*)(efs2:_*)
        finally curBlock = updateEffectForKeys(curBlock,efs2,sm)
      } else {
        // cse? never for effectful stm
        globalDefs.find(n => n.op == s && n.rhs == as) match {
          case Some(n) => n.n
          case None =>
            reflect(Sym(fresh), s, as:_*)()()
        }
      }
    }
  }
  
  def reflect(x: Sym, s: String, as: Def*)(efDeps: Exp*)(efKeys: Exp*): Exp = {
    globalDefs += Node(x,s,as.toList,EffectSummary(efDeps.toList, efKeys.toList))
    x
  }

  /*
  NOTE: There are many problems with the approach of having
  to analyze all lambda arguments to a function to discover
  latent effects. What if the lambda is wrapped in a struct?
  In general we need to find all lambdas potentially reachable
  from an argument. This looks awfully similar to how we
  deal with aliases to mutable variables in LMS 1.x.
  (A possible view here is that tracking lambdas is the same
  as tracking aliases/separation of data structures by means 
  of closure conversion).
  */

  // TODO: x might be a block!
  def getLatentEffect(x: Def) = globalDefs.find(_.n == x) match {
    case Some(Node(_, "λ", List(b @ Block(ins, out, ein, eout)), _)) =>
      getEffKeys(b)
    case _ => 
      Nil
  }


  var curBlock: Effect = _ // could this become an ExplodedStruct?

  type Effect = List[Exp]

  // es the accumulated effect (list of effectful stms) of the current block
  // efs is the set of effect keys of interest
  // return a new effect (subset of es) based on the keys efs
  def effectForKeys(es: Effect, efs: List[Exp]): Effect = {
    es.flatMap { e =>
      globalDefs.find(_.n == e) match {
        case Some(Node(n,op,rhs,eff)) =>
          if ((efs contains n) || (efs intersect eff.keys).nonEmpty) {
            List(e)
          } else {
            effectForKeys(eff.deps, efs)
          }
        case _ => List(e)
      }
    }.distinct
  }

  // remove all dependencies from es (typically curBlock) that are implied 
  // by current node (sm, with effectKeys efs)
  def updateEffectForKeys(es: Effect, efs: List[Exp], sm: Sym): Effect = {
    val prev = effectForKeys(es,efs)
    sm::(es.filterNot(prev.toSet))
    // TODO: is this enough? (when following individual keys?)
  }

  
  // NOTE/TODO: want to mask out purely local effects
  def isPure(b: Block) = b.eff.deps == List(b.ein)

  def getEffKeys0(b: Block): List[Exp] = {
    // TODO -- cleanup: performance!!!

    val bound = new Bound
    bound(Graph(globalDefs.toList,b))

    val nodes = globalDefs.toList.filter(n => bound.hm.getOrElse(n.n,Set())(b.ein))

    // TODO: remove deps on nodes that are bound inside??
    nodes.flatMap(_.eff.keys).distinct
  }

  def getEffKeys(b: Block): List[Exp] = b.eff.keys



  def reify(x: => Exp): Block = {
    val save = curBlock
    try {
      val block = Sym(fresh)
      curBlock = List(block)
      val res = x 
      val b0 = Block(Nil, res, block, EffectSummary(curBlock, Nil)) // XXX FIXME temp 
      Block(Nil, res, block, EffectSummary(curBlock, getEffKeys0(b0)))
    } finally {
      curBlock = save
    }
  }
  def reify(x: Exp => Exp): Block = {
    val save = curBlock
    try {
      val block = Sym(fresh)
      val arg = Sym(fresh)
      curBlock = List(block)
      val res = x(arg) 
      val b0 = Block(arg::Nil, res, block, EffectSummary(curBlock, Nil)) // XXX FIXME temp 
      Block(arg::Nil, res, block, EffectSummary(curBlock, getEffKeys0(b0)))
    } finally {
      curBlock = save
    }
  }

}


case class Graph(val nodes: Seq[Node], val block: Block) {
  // contract: nodes is sorted topologically
}


abstract class Phase extends (Graph => Graph) {

}

// Compute liveness information and discard
// nodes not used in computing the result
class DeadCodeElim extends Phase {
  def apply(g: Graph): Graph = {

    val live = new mutable.HashSet[Sym]

    if (g.block.res.isInstanceOf[Sym])
      live += g.block.res.asInstanceOf[Sym]
    
    for (d <- g.nodes.reverseIterator)
      if (live(d.n)) live ++= syms(d)

    Graph(g.nodes.filter(d => live(d.n)), g.block)
  }
}

// Compute frequency information (i.e., how
// many times a node's result is going to
// be used, in expectation)
class Flow extends Phase {

  val freq = new mutable.HashMap[Sym,Double]

  // XXX: not clear how to count effect deps
  // (e.g. 'if' shouldn't count effect as 0.5, b/c together with
  // a normal dep this is 1.0, and would thus hoist stuff)

  // XXX perhaps we need to count freqs differently (?)

  def symsFreq(x: Node): List[(Def,Double)] = x match {
    case Node(f, "λ", List(Block(in, y, ein, eff)), _) => 
      List((y,100.0)) ++ eff.deps.map(e => (e,0.001))
    case Node(_, "?", c::Block(ac,ae,ai,af)::Block(bc,be,bi,bf)::_, _) => 
      List((c,1.0),(ae,0.5),(be,0.5)) ++ af.deps.map(e => (e,0.001)) ++ bf.deps.map(e => (e,0.001))
    case _ => syms(x) map (s => (s,1.0))
  }

  def apply(g: Graph): Graph = {

    if (g.block.res.isInstanceOf[Sym])
      freq(g.block.res.asInstanceOf[Sym]) = 1.0

    for (e <- g.block.eff.deps)
      if (e.isInstanceOf[Sym])
        freq(e.asInstanceOf[Sym]) = 1.0

    for (d <- g.nodes.reverseIterator) {
      if (freq contains d.n) {
        val s = freq(d.n)
        for ((e:Sym,f) <- symsFreq(d))
          if (f > 0.5) freq(e) = (freq.getOrElse(e, 0.0) + (f*s))
      }
    }

    //freq.toList.sortBy(_._1.n).foreach(println)

    g
  }
}

// Compute bound structure information 
// (i.e., which bound variables a node depends on)
class Bound extends Phase {

  val hm = new mutable.HashMap[Sym,Set[Sym]]

  def apply(g: Graph): Graph = {
    val bound = g.nodes.flatMap(boundSyms).toSet ++ g.block.bound

    // for recursive syms, we don't want to force
    // non-recursive deps into nested scopes
    for (Node(b,"λ",_,_) <- g.nodes)
      hm(b) = Set()

    for (b <- bound) 
      hm(b) = Set(b)

    for (d <- g.nodes) {
      val b = boundSyms(d).toSet - d.n
      hm(d.n) = syms(d).flatMap(a => hm.getOrElse(a,Set(a))).toSet -- b
    }

    //hm.foreach(println)

    g
  }

}


abstract class Traverser {

  val bound = new Bound

  var path = List[Sym]()
  var inner: Seq[Node] = _

  def withScope[T](p: List[Sym], ns: Seq[Node])(b: =>T): T = {
    val (path0, inner0) = (path, inner)
    path = p; inner = ns;
    try b finally { path = path0; inner = inner0 }
  }

  def traverse(y: Block, extra: Sym*): Unit = {
    val path1 = y.bound ++ extra.toList ++ path

    // a node is available if all bound vars
    // it depends on are in scope
    def available(d: Node) = 
      bound.hm(d.n) -- path1 - d.n == Set()

    // freq/block computation
    def symsFreq(x: Node): List[(Def,Double)] = x match {
      case Node(f, "λ", List(Block(in, y, ein, eff)), _) => 
        (y::eff.deps).map(e => (e,100.0))
      // case Node(_, "?", c::Block(ac,ae,af)::Block(bc,be,bf)::Nil, _) => 
        // List((c,1.0)) ++ (ae::be::af ++ bf).map(e => (e,0.5))
      case Node(_, "?", c::Block(ac,ae,ai,af)::Block(bc,be,bi,bf)::Nil, eff) => 
        (c::eff.deps).map(e => (e,1.0)) ++ (ae::be::af.deps ++ bf.deps).map(e => (e,0.5)) // XXX why eff.deps? would lose effect-only statements otherwise!
      case Node(_, "W", Block(ac,ae,ai,af)::Block(bc,be,bi,bf)::Nil, eff) => 
        eff.deps.map(e => (e,1.0)) ++ (ae::be::af.deps ++ bf.deps).map(e => (e,100.0)) // XXX why eff.deps?
      case _ => syms(x) map (s => (s,1.0))
    }

    // find out which nodes are reachable on a
    // warm path (not only via if/else branches)
    val g = new Graph(inner, y)

    val reach = new mutable.HashSet[Sym]

    if (g.block.res.isInstanceOf[Sym])
      reach += g.block.res.asInstanceOf[Sym]

    for (e <- g.block.eff.deps)
      if (e.isInstanceOf[Sym])
        reach += e.asInstanceOf[Sym]

    // for (d <- g.nodes) {
    //   println("check "+d + " " + bound.hm(d.n) + " " + path1.toSet + " " + reach(d.n) + " " + available(d))
    // }

    for (d <- g.nodes.reverseIterator) {
      if ((reach contains d.n)) {
        if (available(d)) { 
          // node will be sched here, don't follow if branches!
          for ((e:Sym,f) <- symsFreq(d) if f > 0.5) reach += e
        } else {
          for ((e:Sym,f) <- symsFreq(d)) reach += e
        }
      }
    }

    // Should node d be scheduled here? It must be:
    // (1) available: not dependent on other bound vars
    // (2) used at least as often as the block result

    def scheduleHere(d: Node) =
      available(d) && reach(d.n)

    val (outer1, inner1) = inner.partition(scheduleHere)

    withScope(path1, inner1) {
      traverse(outer1, y)
    }
  }

  def traverse(ns: Seq[Node], res: Block): Unit = {
    ns.foreach(traverse)
  }

  def traverse(n: Node): Unit = n match {
    case n @ Node(f, "λ", List(y:Block), _) => 
      // special case λ: add free var f
      traverse(y, f)
    case n @ Node(f, op, es, _) =>
      // generic traversal: go into all blocks
      for (e @ Block(_,_,_,_) <- es)
        traverse(e)
  }

  def apply(g: Graph): Unit = {

    bound(g)

    withScope(Nil, g.nodes) {
      traverse(g.block)
    }
  }

}

object utils {
    // XXX do without
  def captureOut(func: => Any): String = {
    val source = new java.io.ByteArrayOutputStream()
    withOutput(new java.io.PrintStream(source))(func)
    source.toString    
  }
  def withOutput[T](out: java.io.PrintStream)(f: => Unit): Unit = {
    scala.Console.withOut(out)(scala.Console.withErr(out)(f))
  }
}

class CodeGen extends Traverser {

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

class ScalaCodeGen extends Traverser {

  def emit(s: String) = println(s)

  def quote(s: Def): String = s match {
    case Sym(n) => s"x$n"
    case Const(x: String) => "\""+x+"\""
    case Const(x) => x.toString
  }

  override def traverse(ns: Seq[Node], y: Block): Unit = {
    super.traverse(ns, y)
    emit(quote(y.res))
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block),_) => 
      val x = y.in.head
      emit(s"def ${quote(f)}(${quote(x)}: Int): Int = {")
      // see what becomes available given new bound vars
      traverse(y, f)
      emit(s"}")
    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) => 
      emit(s"val $f = if (${quote(c)}) {")
      traverse(a)
      emit(s"} else {")
      traverse(b)
      emit(s"}")
    case n @ Node(f,"W",List(c:Block,b:Block,e),_) => 
      emit(s"while ({")
      traverse(c)
      emit(s"}) else {")
      traverse(b)
      emit(s"}")
    case n @ Node(s,"+",List(x,y),_) => 
      emit(s"val $s = ${quote(x)} + ${quote(y)}")
    case n @ Node(s,"-",List(x,y),_) => 
      emit(s"val $s = ${quote(x)} - ${quote(y)}")
    case n @ Node(s,"*",List(x,y),_) => 
      emit(s"val $s = ${quote(x)} * ${quote(y)}")
    case n @ Node(s,"/",List(x,y),_) => 
      emit(s"val $s = ${quote(x)} / ${quote(y)}")
    case n @ Node(s,"%",List(x,y),_) => 
      emit(s"val $s = ${quote(x)} % ${quote(y)}")
    case n @ Node(s,"==",List(x,y),_) => 
      emit(s"val $s = ${quote(x)} == ${quote(y)}")
    case n @ Node(s,"!=",List(x,y),_) => 
      emit(s"val $s = ${quote(x)} != ${quote(y)}")
    case n @ Node(s,"var_new",List(x),_) => 
      emit(s"var $s = ${quote(x)}")
    case n @ Node(s,"var_get",List(x),_) => 
      emit(s"val $s = ${quote(x)}")
    case n @ Node(s,"var_set",List(x,y),_) => 
      emit(s"${quote(x)} = ${quote(y)}")
    case n @ Node(s,"array_new",List(x),_) => 
      emit(s"var $s = new Array[Int](${quote(x)})")
    case n @ Node(s,"array_get",List(x,i),_) => 
      emit(s"val $s = ${quote(x)}(${quote(i)})")
    case n @ Node(s,"array_set",List(x,i,y),_) => 
      emit(s"${quote(x)}(${quote(i)}) = ${quote(y)}")
    case n @ Node(s,"@",x::y::_,_) => 
      emit(s"val $s = ${quote(x)}(${quote(y)})")
    case n @ Node(s,"P",List(x),_) => 
      emit(s"val $s = println(${quote(x)})")
    case n @ Node(_,_,_,_) => 
      emit(s"??? " + n.toString)
  }
}


class CompactScalaCodeGen extends Traverser {

  val rename = new mutable.HashMap[Sym,String]

  var doRename = false
  var doPrintEffects = false

  def emit(s: String) = println(s)

  override def traverse(ns: Seq[Node], y: Block): Unit = {

    // check if a node is used from some inner scope
    val hmi = new mutable.HashSet[Sym]
    for (n <- inner) {
      hmi ++= syms(n)
    }

    // ----- forward pass -----

    // lookup sym -> node for locally defined nodes
    val df = new mutable.HashMap[Sym,Node]

    // how many times a sym is used locally (excl blocks and effects)
    val hm = new mutable.HashMap[Sym,Int]

    // local successor nodes (incl blocks and effects)
    val succ = new mutable.HashMap[Sym,List[Sym]]    

    // count how many times a node is used at the current level
    if (y.res.isInstanceOf[Sym]) hm(y.res.asInstanceOf[Sym]) = 1
    for (n <- ns) {
      df(n.n) = n
      for (s <- directSyms(n) if df.contains(s)) // do not count refs through blocks or effects
        hm(s) = hm.getOrElse(s,0) + 1
      for (s <- syms(n) if df.contains(s))
        succ(s) = n.n::succ.getOrElse(s,Nil)
    }

    val dis = new mutable.HashSet[Sym]

    val save = shouldInline

    // should a definition be inlined or let-inserted?
    shouldInline = { (n: Sym) => 
      if ((df contains n) &&              // locally defined
          (hm.getOrElse(n, 0) == 1) &&    // locally used exactly once
          (!hmi(n)))                      // not used in nested scopes
          Some(df(n))
      else None }


    // ----- backward pass -----

    val seen = new mutable.HashSet[Sym]

    def processNodeHere(n: Node): Unit = {
      seen += n.n
      for (s <- directSyms(n).reverse) {
        checkInline(s)
      }
    }

    def checkInline(res: Sym) = shouldInline(res) match { 
      case Some(n) =>
        // want to inline, now check that all successors are already there, else disable
        if (succ.getOrElse(n.n,Nil).forall(seen))
          processNodeHere(n)
        else
          df -= n.n
      case _ =>
    }

    if (y.res.isInstanceOf[Sym])
      checkInline(y.res.asInstanceOf[Sym]) // try to inline y.res, state after must be y.eff

    for (n <- ns.reverse) {
      if (shouldInline(n.n).isEmpty) {
        processNodeHere(n)
      }
    }


    // ----- forward pass -----

    // only emit statements if not inlined
    for (n <- ns) {
      if (shouldInline(n.n).isEmpty)
        traverse(n)
    }

    print(shallow(y.res) + quoteEff(y.eff))

    shouldInline = save
  }

  var shouldInline: Sym => Option[Node] = (_ => None)

  object InlineSym {
    def unapply(x: Sym) = shouldInline(x)
  }

  def quote(s: Def): String = s match {
    case s @ Sym(n) if doRename => rename.getOrElseUpdate(s, s"x${rename.size}")
    case Sym(n) => s.toString
    case Const(x: String) => "\""+x+"\""
    case Const(x) => x.toString
    // case Eff(x) => x.map(quote).mkString("")
  }

  def shallow(n: Def): String = n match {
    case InlineSym(n) => shallow(n)
    case b:Block => quoteBlock1(b)
    case _ => quote(n)
  }

  def quoteEff(x: Def): String = 
    if (!doPrintEffects) "" 
    else " /* " + quote(x) + " */"

  def quoteEff(x: List[Exp]): String = 
    if (!doPrintEffects) "" 
    else " /* " + x.map(quote).mkString("") + " */"

  def quoteEff(x: EffectSummary): String = quoteEff(x.deps)

  def quoteEff(n: Node): String = if (!doPrintEffects) "" else {
    val deps = n.eff.deps
    val eff = n.eff.keys
    if (deps.isEmpty && eff.isEmpty) "" else {
      s"/* val ${quote(n.n)} = ${eff.map(quote).mkString(",")}:${deps.map(quote).mkString("")} */"
    }
  }

  // def quoteEff(x: Effect) = 
  //   if (!doPrintEffects || x.isEmpty) ""
  //   else " /* " + quote(x) + " */"


  // XXX NOTE: performance implications of capture + concat !!!
  // XXX TODO: what if block is empty?
  def quoteBlock(f: => Unit) = {
    val b = utils.captureOut(f)
    if (b contains '\n')
      "{\n" + b + "\n}"
    else
      b
  }
  def quoteBlock1(y: Block, argType: Boolean = false) = {
    def eff = quoteEff(y.ein)
    if (y.in.length == 0) {
      val b = utils.captureOut(traverse(y))
      if (b contains '\n')
        s"{$eff\n" + eff + b + "\n}"
      else
        b
    } else if (y.in.length == 1) {
      val x = y.in.head
      val b = utils.captureOut(traverse(y))
      def typed(s:String) = if (argType) s+": Int" else s //FIXME hardcoded
      def paren(s:String) = if (argType) "("+s+")" else s
      if (b contains '\n')
        paren(s"{ ${typed(quote(x))}$eff => \n$b\n}")
      else
        s"(${paren(typed(quote(x)))}$eff => $b)"
    } else (???) //FIXME
  }

  // generate string for node's right-hand-size
  // (either inline or as part of val def)
  // XXX TODO: precedence of nested expressions!!
  def shallow(n: Node): String = (n match {
    case n @ Node(f,"λ",List(y:Block),_) => 
      // XXX what should we do for functions? 
      // proper inlining will likely work better 
      // as a separate phase b/c it may trigger
      // further optimizations
      quoteBlock1(y, true)
    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) => 
      s"if (${shallow(c)}) " +
      quoteBlock(traverse(a)) +
      s" else " +
      quoteBlock(traverse(b))
    case n @ Node(f,"W",List(c:Block,b:Block),_) => 
      s"while (" +
      quoteBlock(traverse(c)) +
      s") " +
      quoteBlock(traverse(b))
    case n @ Node(s,"+",List(x,y),_) => 
      s"${shallow(x)} + ${shallow(y)}"
    case n @ Node(s,"-",List(x,y),_) => 
      s"${shallow(x)} - ${shallow(y)}"
    case n @ Node(s,"*",List(x,y),_) => 
      s"${shallow(x)} * ${shallow(y)}"
    case n @ Node(s,"/",List(x,y),_) => 
      s"${shallow(x)} / ${shallow(y)}"
    case n @ Node(s,"%",List(x,y),_) => 
      s"${shallow(x)} % ${shallow(y)}"
    case n @ Node(s,"==",List(x,y),_) => 
      s"${shallow(x)} == ${shallow(y)}"
    case n @ Node(s,"!=",List(x,y),_) => 
      s"${shallow(x)} != ${shallow(y)}"
    case n @ Node(s,"var_get",List(x),_) => 
      s"${shallow(x)}"
    case n @ Node(s,"array_get",List(x,i),_) => 
      s"${shallow(x)}(${shallow(i)})"
    case n @ Node(s,"@",x::y::_,_) => 
      s"${shallow(x)}(${shallow(y)})"
    case n @ Node(s,"P",List(x),_) => 
      s"println(${shallow(x)})"
    case n @ Node(s,"comment",Const(str: String)::Const(verbose: Boolean)::(b:Block)::_,_) => 
      quoteBlock {
        emit("//#" + str)
        if (verbose) {
          emit("// generated code for " + str.replace('_', ' '))
        } else {
          emit("// generated code")
        }
        traverse(b)
        emit("//#" + str)
      }

    case n @ Node(_,op,args,_) => 
      s"$op(${args.map(shallow).mkString(", ")})"
  }) + quoteEff(n)

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block),_) => 
      val x = y.in.head
      emit(s"def ${quote(f)}(${quote(x)}: Int): Int${quoteEff(y.ein)} = ${ quoteBlock(traverse(y,f)) }")
    case n @ Node(s,"P",_,_) => // Unit result
      emit(shallow(n))
    case n @ Node(s,"W",_,_) => // Unit result
      emit(shallow(n))
    case n @ Node(s,"var_new",List(x),_) => 
      emit(s"var ${quote(s)} = ${shallow(x)}")
    case n @ Node(s,"var_set",List(x,y),_) => 
      emit(s"${quote(x)} = ${shallow(y)}")
    case n @ Node(s,"array_new",List(x),_) => 
      emit(s"val ${quote(s)} = new Array[Int](${shallow(x)})")
    case n @ Node(s,"array_set",List(x,i,y),_) => 
      emit(s"${shallow(x)}(${shallow(i)}) = ${shallow(y)}")
    case n @ Node(s,op,_,_) if op.startsWith("effect-") => 
      emit(s"/* val ${quote(s)} = " + shallow(n) + "*/") 
    case n @ Node(s,_,_,_) => 
      emit(s"val ${quote(s)} = " + shallow(n)) 
  }

  override def apply(g: Graph) = {
    super.apply(g)
    println
  }
}


abstract class Transformer extends Traverser {

  var g: GraphBuilder = null

  val subst = new mutable.HashMap[Sym,Exp]

  def transform(s: Exp): Exp = s match {
    case s @ Sym(_) if subst contains s => subst(s)
    case s @ Sym(_) => println(s"Warning: not found in subst $subst: "+s); s
    case a => a // must be const
  }

  def transform(b: Block): Block = b match {
    case b @ Block(Nil, res, block, eff) =>
      g.reify { 
        //subst(block) = g.effectToExp(g.curBlock) //XXX
        traverse(b); transform(res) 
      }
    case b @ Block(arg::Nil, res, block, eff) =>
      g.reify { e =>
        if (subst contains arg)
          println(s"Warning: already have a subst for $arg")
        try {
          subst(arg) = e
          //subst(block) = g.effectToExp(g.curBlock) //XXX
          traverse(b)
          transform(res)
        } finally subst -= arg
      }
    case _ => ???
  }

  def transform(n: Node): Exp = n match {
    case Node(s,"λ", List(b @ Block(in, y, ein, eff)),_) =>
      // need to deal with recursive binding!
      val s1 = Sym(g.fresh)
      subst(s) = s1
      g.reflect(s1, "λ", transform(b))()()
    case Node(s,op,rs,es) => 
      // effect dependencies in target graph are managed by
      // graph builder, so we drop all effects here
      val (effects,pure) = (es.deps,rs)
      val args = pure.map {
        case b @ Block(_,_,_,_) =>
          transform(b)
        case s : Exp => 
          transform(s)
        case a => 
          a
      }
      // NOTE: we're not transforming 'effects' here (just the keys)
      if (effects.nonEmpty)
        g.reflectEffect(op,args:_*)(es.keys.map(transform):_*)
      else
        g.reflect(op,args:_*)
  }

  override def traverse(n: Node): Unit = {
    subst(n.n) = transform(n)
    // println(s"transformed ${n.n}->${subst(n.n)}")
  }

  def transform(graph: Graph): Graph = {
    // XXX unfortunate code duplication, either
    // with traverser or with transform(Block)
    val block = g.reify { e => 
      assert(graph.block.in.length == 1)
      subst(graph.block.in(0)) = e
      // subst(graph.block.ein) = g.curBlock.head // XXX 
      super.apply(graph); transform(graph.block.res) }
    Graph(g.globalDefs,block)
  }

}


class FrontEnd {

  var g: GraphBuilder = null

  val CTRL = Const("CTRL")
  val STORE = Const("STORE")

  case class BOOL(x: Exp) {
    //def &&(y: => BOOL): BOOL = BOOL(g.reflect("&",x,y.x)) // should call if?
    //def ||(y: => BOOL): BOOL = BOOL(g.reflect("|",x,y.x))
    def unary_! = BOOL(g.reflect("!",x))
  }

  case class INT(x: Exp) {
    def +(y: INT): INT = INT(g.reflect("+",x,y.x))
    def -(y: INT): INT = INT(g.reflect("-",x,y.x))
    def *(y: INT): INT = INT(g.reflect("*",x,y.x))
    def /(y: INT): INT = INT(g.reflect("/",x,y.x))
    def %(y: INT): INT = INT(g.reflect("%",x,y.x))
    def ===(y: INT): BOOL = BOOL(g.reflect("==",x,y.x))
    def !==(y: INT): BOOL = BOOL(g.reflect("!=",x,y.x))
  }

  case class STRING(x: Exp) {
  }

  case class ARRAY(x: Exp) {
    def apply(i: INT): INT = INT(g.reflectEffect("array_get",x,i.x)(x))
    def update(i: INT, y: INT): Unit = g.reflectEffect("array_set",x,i.x,y.x)(x)
  }
  object ARRAY {
    def apply(n: INT): ARRAY = ARRAY(g.reflectEffect("array_new",n.x)(STORE))
  }

  case class VAR(x: Exp) {
    def apply(): INT = INT(g.reflectEffect("var_get",x)(x))
    def update(y: INT): Unit = g.reflectEffect("var_set",x,y.x)(x)
  }
  object VAR {
    def apply(x: INT): VAR = VAR(g.reflectEffect("var_new",x.x)(STORE))
  }

  def IF(c: BOOL)(a: => INT)(b: => INT): INT = {
    val aBlock = g.reify(a.x)
    val bBlock = g.reify(b.x)
    // compute effect (aBlock || bBlock)
    val pure = g.isPure(aBlock) && g.isPure(bBlock)
    val efs = (g.getEffKeys(aBlock) ++ g.getEffKeys(bBlock)).distinct
    if (pure)
      INT(g.reflect("?",c.x,aBlock,bBlock))
    else
      INT(g.reflectEffect("?",c.x,aBlock,bBlock)(efs:_*))
  }

  def WHILE(c: => BOOL)(b: => Unit): Unit = {
    val cBlock = g.reify(c.x)
    val bBlock = g.reify({b;Const(())})
    // compute effect (cBlock bBlock)* cBlock
    val efs = (g.getEffKeys(cBlock) ++ g.getEffKeys(bBlock)).distinct
    g.reflectEffect("W",cBlock,bBlock)(efs:_*)
  }

  
  def APP(f: Exp, x: INT): INT = {
    // XXX lookup lambda ...
    g.globalDefs.find(_.n == f) match {
      case Some(Node(f, "λ", List(b: Block), _)) =>
        if (g.isPure(b))
          INT(g.reflect("@",f,x.x))
        else 
          INT(g.reflectEffect("@",f,x.x)(g.getEffKeys(b):_*))
      case _ =>
        INT(g.reflectEffect("@",f,x.x)(CTRL))
    }
  }

  def PRINT(x: INT): Unit =
    g.reflectEffect("P",x.x)(CTRL)

  def PRINT(x: STRING): Unit =
    g.reflectEffect("P",x.x)(CTRL)

  def FUN(f: INT => INT): INT => INT = FUN((_,x) => f(x))

  def FUN(f: ((INT=>INT),INT) => INT): INT => INT = {
    val fn = Sym(g.fresh)
    //val xn = Sym(g.fresh)
    val f1 = (x: INT) => APP(fn,x)
    // NOTE: lambda expression itself does not have
    // an effect, so body block should not count as 
    // latent effect of the lambda
    g.reflect(fn,"λ",g.reify(xn => f(f1,INT(xn)).x))()()
    f1
  }

  implicit def liftInt(x: Int): INT = INT(Const(x))
  implicit def liftBool(x: Boolean): BOOL = BOOL(Const(x))
  implicit def liftString(x: String): STRING = STRING(Const(x))

  def mkGraphBuilder() = new GraphBuilder

  def program(body: INT => INT): Graph = {
    assert(g == null)
    g = mkGraphBuilder()
    try {
      val block = g.reify { arg => body(INT(arg)).x }
      Graph(g.globalDefs, block)
    } finally g = null
  }


}


// DONE: frequency-based code motion

// DONE: local liveness and compact printer

// DONE: canonicalize names in generated code

// DONE: more functionality: Bool, String, Array, Var, While, ...

// DONE: transformers

// DONE: CSE

// DONE: smart constructors

// DONE: fusion

// DONE: tensor case study

// DONE: macro front-end to generate lowering

// DONE: fine-grained effects 


// TODO: front end: parametric types, type classes

// TODO: metadata: source locations, types, etc

// TODO: error reporting architecture

// TODO: mutual recursion, memoization for functions

// TODO: aliasing, ownership, hard and soft deps

// TODO: staticData

// TODO: ExplodedStruct

// TODO: more sophisticated transformers (worklist, register rewrites, etc)

// TODO: parallellization with OpenMP

// TODO: lms tutorials & more ...

// TODO: low-level:
// - type checker
// - CPS conversion
// - closure conversion
// - register allocation
// - x86 assembly

