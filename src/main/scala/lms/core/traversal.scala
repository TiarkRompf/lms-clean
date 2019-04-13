package lms.core

import scala.collection.mutable

import Backend._

abstract class Traverser {

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

/*
    NOTES ON RECURSIVE SCHEDULES

    // E.g. from tutorials/AutomataTest:
    //    x2 = fwd; x23 = DFAState(x2); x2' = (λ ...); x23

    // PROBLEM: Since we iterate over g.nodes in reverse order, the lambda (x2')
    // is initially not reachable (only the fwd node x2)!

    // Ideally we would directly want to build something like
    //    x23 = fwd; x2 = (λ ...) ; x23 = DFAState(x2)
    // but this seems hard to achieve in general.

    // We fixed it so far by using a different symbol inside and outside the
    // function, so now we get:
    //    x2 = fwd; x23 = DFAState(x2); x2' = (λ ...) ; x33 = DFAState(x2')

    // This is less ideal in terms of CSE, but at least we get a correct
    // schedule.

    // We could improve this by using a proper call to SCC instead of just
    // iterating over g.nodes in reverse order. The performance implications
    // aren't clear, to we decided to postponse this.

    // Code would look like this:

    def find(s: Sym) = g.nodes.reverse.find(_.n == s).toList
    def succ(s: Sym) = {
      find(s).flatMap { d =>
      if (available(d)) symsFreq(d) collect { case (e:Sym,f) if f > 0.5 => e }
      else symsFreq(d) collect { case (e:Sym,f) => e }
    }}

    val nodes1 = lms.util.GraphUtil.stronglyConnectedComponents(g.block.used, succ)

    // nodes1.foreach(println)

    // def tb(x: Boolean) = if (x) 1 else 0
    // for (n <- inner) {
    //   println(s"// ${tb(available(n))} ${tb(reach(n.n))} $n ${symsFreq(n)}")
    // }
*/


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


class CompactTraverser extends Traverser {

  def mayInline(n: Node): Boolean = n match {
    case Node(s, "var_new", _, _) => false
    case _ => true
  }

  var shouldInline: Sym => Option[Node] = (_ => None)
  var numStms = 0

  object InlineSym {
    def unapply(x: Sym) = shouldInline(x)
  }

  override def withScope[T](p: List[Sym], ns: Seq[Node])(b: =>T): T = {
    val save = shouldInline
    val save1 = numStms
    try super.withScope(p, ns)(b) finally { shouldInline = save; numStms = save1 }
  }

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
      for (s <- directSyms(n) if df.contains(s) || n.op == "λforward") // do not count refs through blocks or effects
        hm(s) = hm.getOrElse(s,0) + 1                                  // NOTE: λforward is to deal with recursive defs
      for (s <- syms(n) if df.contains(s))
        succ(s) = n.n::succ.getOrElse(s,Nil)
    }

    val dis = new mutable.HashSet[Sym]

    // should a definition be inlined or let-inserted?
    shouldInline = { (n: Sym) => 
      if ((df contains n) &&              // locally defined
          (hm.getOrElse(n, 0) == 1) &&    // locally used exactly once
          (!hmi(n)))                      // not used in nested scopes
          Some(df(n))
      else None }
    // (shouldInline is protected by withScope)


    // ----- backward pass -----

    // for nodes that should be inlined, disable if dependencies interfere
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
        if (mayInline(n) && succ.getOrElse(n.n,Nil).forall(seen))
          processNodeHere(n)
        else
          df -= n.n
      case _ =>
    }

    if (y.res.isInstanceOf[Sym])
      checkInline(y.res.asInstanceOf[Sym]) // try to inline y.res, state after must be y.eff

    numStms = 0

    for (n <- ns.reverse) {
      if (shouldInline(n.n).isEmpty) {
        processNodeHere(n); numStms += 1
      }
    }


    // ----- forward pass -----

    traverseCompact(ns, y)
  }

  def traverseCompact(ns: Seq[Node], y: Block): Unit = {
    // only emit statements if not inlined
    for (n <- ns) {
      if (shouldInline(n.n).isEmpty)
        traverse(n)
    }
  }

  // subclass responsibility:

  // -- disabled here because don't want to fix result type
  def traverseShallow(n: Def): Unit = n match {
    case InlineSym(n) => traverseShallow(n)
    case b:Block => traverse(b)
    case _ =>
  }

  def traverseShallow(n: Node): Unit = n match {
    case n @ Node(_,op,args,_) =>
      args.foreach(traverseShallow)
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block),_) =>
      // special case λ: add free var f
      traverse(y,f)
    case n @ Node(f, op, es, _) =>
      // generic traversal
      es.foreach(traverseShallow)
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
