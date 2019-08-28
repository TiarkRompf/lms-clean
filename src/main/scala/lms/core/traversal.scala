package lms.core

import scala.collection.mutable

import Backend._
import lms.core.stub.Adapter

abstract class Traverser {

  // freq/block computation
  def symsFreq(x: Node): Set[(Def,Double)] = x match {
    case Node(f, "λ", Block(in, y, ein, eff)::_, _) =>
      eff.deps.map((e: Def) => (e,100.0)) + ((y, 100.0))
    // case Node(_, "?", c::Block(ac,ae,af)::Block(bc,be,bf)::Nil, _) =>
      // List((c,1.0)) ++ (ae::be::af ++ bf).map(e => (e,0.5))
    case Node(_, "?", c::(a: Block)::(b: Block)::_, eff) =>
      eff.hdeps.map((e: Def) => (e,1.0)) + ((c, 1.0)) ++ (a.used ++ b.used).map((e: Def) => (e,0.5)) // XXX why eff.deps? would lose effect-only statements otherwise!
    case Node(_, "W", (a: Block)::(b: Block)::_, eff) =>
      eff.hdeps.map((e: Def) => (e,1.0)) ++ (a.used ++ b.used).map(e => (e,100.0)) // XXX why eff.deps?
    case Node(_, "switch", guard::rhs, eff) => // 1 / # blocks instead of 0.5?
      var freqs = Set[(Def,Double)]()
      rhs.foreach {
        case b: Block => freqs ++= b.used.map(e => (e, 0.5))
        case _ =>
      }
      freqs + ((guard, 1.0)) ++ eff.hdeps.map((e: Def) => (e,1.0))
    case _ => hardSyms(x).map((s: Def) => (s,1.0))
  }


  val bound = new Bound

  var path = List[Sym]()
  var inner: Seq[Node] = _
  val blockEffectPath = new mutable.HashMap[Block,Set[Exp]]

  def withScope[T](p: List[Sym], ns: Seq[Node])(b: =>T): T = {
    val (path0, inner0) = (path, inner)
    path = p; inner = ns;
    try b finally { path = path0; inner = inner0 }
  }

  def withScope1[T](p: List[Sym], ns: Seq[Node])(b: (List[Sym], Seq[Node]) => T): T = {
    val (path0, inner0) = (path, inner)
    path = p; inner = ns;
    try b(path0, inner0) finally { path = path0; inner = inner0 }
  }

  def withResetScope[T](p: List[Sym], ns: Seq[Node])(b: =>T): T = {
    assert(path.takeRight(p.length) == p, s"$path -- $p")
    val inner0 = inner
    inner = ns
    try b finally { inner = inner0 }

  }

  def focus[T](y: Block, extra: Sym*)(f: (Seq[Node], Block) => T): T =
    focus0(y, extra: _*) { (path1, inner1, outer1, y) =>
      withScope(path1, inner1) {
        f(outer1, y)
      }
    }

  def focus0[T](y: Block, extra: Sym*)(f: (List[Sym], Seq[Node], Seq[Node], Block) => T): T = {
    val path1 = y.bound ++ extra.toList ++ path

    // a node is available if all bound vars
    // it depends on are in scope
    def available(d: Node) =
      bound.hm(d.n) -- path1 - d.n == Set()

    // find out which nodes are reachable on a
    // warm path (not only via if/else branches)
    val g = new Graph(inner, y, null)

    val reach = new mutable.HashSet[Sym]
    val reachInner = new mutable.HashSet[Sym]

    reach ++= y.used

    // for (d <- g.nodes) {
    //   println("check "+d + " " + bound.hm(d.n) + " " + path1.toSet + " " + reach(d.n) + " " + available(d))
    // }

    for (d <- g.nodes.reverseIterator) {
      if ((reach contains d.n)) {
        if (available(d)) {
          // node will be sched here, don't follow if branches!
          // other statement will be scheduled in an inner block
          for ((e:Sym,f) <- symsFreq(d))
            if (f > 0.5) reach += e else reachInner += e
        } else {
          reach ++= hardSyms(d)
        }
      }
      if (reachInner(d.n)) {
        reachInner ++= hardSyms(d)
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
    // aren't clear, to we decided to postpone this.

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

    var outer1 = Seq[Node]()
    var inner1 = Seq[Node]()

    // Extra reachable statement from soft dependencies
    val extraThroughSoft = new mutable.HashSet[Sym]
    for (n <- inner.reverseIterator) {
      if (reach(n.n) || extraThroughSoft(n.n)) {
        if (available(n)) {
          outer1 = n +: outer1
          if (!reach(n.n)) // if added through soft deps, hards needs to be added as well
            extraThroughSoft ++= syms(n)
          else
            extraThroughSoft ++= n.eff.sdeps
        } else {
          inner1 = n +: inner1
        }
      } else if (reachInner(n.n)) {
        inner1 = n +: inner1
      }
    }

    // System.out.println(s"================ $y ==============")
    // for (n <- inner)
    //   System.out.println(s"\t$n")
    // System.out.println(s"==== Outer ====")
    // for (n <- outer1)
    //   System.out.println(s"\t$n")
    // System.out.println(s"==== inner ====")
    // for (n <- inner1)
    //   System.out.println(s"\t$n")

    f(path1, inner1, outer1, y)
  }

  def traverse(ns: Seq[Node], res: Block): Unit = {
    ns.foreach(traverse)
  }

  def traverse(b: Block, extra: Sym*): Unit = {
    focus(b, extra:_*)(traverse)
  }

  def getFreeVarBlock(y: Block, extra: Sym*): Set[Sym] = focus(y, extra:_*) { (ns: Seq[Node], res: Block) =>
    val used = new mutable.HashSet[Sym]
    val bound = new mutable.HashSet[Sym]
    used ++= y.used
    bound ++= path
    for (n <- inner) {
      used ++= syms(n)
      bound += n.n
      bound ++= boundSyms(n)
    }
    for (n <- ns) {
      used ++= syms(n)
      bound += n.n
      bound ++= boundSyms(n)
    }
    (used diff bound).toSet
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

abstract class CPSTraverser extends Traverser {

  def traverse(y: Block, extra: Sym*)(k: Exp => Unit): Unit =
    focus0(y, extra: _*) { (path1, inner1, outer1, y) =>
      withScope1(path1, inner1) { (path0, inner0) =>
        traverse(outer1, y){ v => withScope(path0, inner0)(k(v)) }
      }
    }

  def traverse(ns: Seq[Node], y: Block)(k: Exp => Unit): Unit = {
    if (!ns.isEmpty) traverse(ns.head)(traverse(ns.tail, y)(k)) else k(y.res)
  }

  def traverse(bs: List[Block])(k: => Unit): Unit =
    if (!bs.isEmpty) traverse(bs.head)(v => traverse(bs.tail)(k)) else k

  def traverse(n: Node)(k: => Unit): Unit = n match {
    case n @ Node(f, "λ", List(y:Block), _) =>
      traverse(y, f)(v => k)
    case n @ Node(f, op, es, _) =>
      val blocks = es.filter{ case Block(_,_,_,_) => true; case _ => false}.map(_.asInstanceOf[Block])
      traverse(blocks)(k)
  }

  def apply(g: Graph)(k: Int): Unit = {
    bound(g)
    path = Nil; inner = g.nodes
    traverse(g.block)(e => {})
  }
}


class CompactTraverser extends Traverser {

  def mayInline(n: Node): Boolean = n match {
    case Node(s, "var_new", _, _) => false
    case Node(s, "local_struct", _, _) => false
    case Node(s, "timestamp", _, _) => false
    case Node(s, "NewArray", List(_, _), _) => false
    case Node(s, "Array", _, _) => false
    case Node(s, "String.##", List(_, _), _) => false
    case Node(s, "comment", _, _) => false
    case _ => true
  }

  var shouldInline: Sym => Option[Node] = (_ => None)
  var numStms = 0
  var lastNode: Option[Node] = None

  object InlineSym {
    def unapply(x: Sym) = shouldInline(x)
  }

  override def withScope[T](p: List[Sym], ns: Seq[Node])(b: =>T): T = {
    val save = shouldInline
    val save1 = numStms
    val save2 = lastNode
    try super.withScope(p, ns)(b) finally { shouldInline = save; numStms = save1; lastNode = save2 }
  }

  override def traverse(ns: Seq[Node], y: Block): Unit = {

    // ----- forward pass -----

    // lookup sym -> node for locally defined nodes
    val df = new mutable.HashMap[Sym,Node]

    // how many times a sym is used locally (excl blocks and effects)
    val hm = new mutable.HashMap[Sym,Int]

    // local successor nodes (incl blocks and effects)
    val succ = new mutable.HashMap[Sym,List[Sym]]

    // check if a node is used from some inner scope
    val hmi = new mutable.HashSet[Sym]

    // count how many times a node is used at the current level
    if (y.res.isInstanceOf[Sym]) hm(y.res.asInstanceOf[Sym]) = 1
    for (n <- ns) {
      df(n.n) = n
      for (s <- directSyms(n) if df.contains(s) || n.op == "λforward") // do not count refs through blocks or effects
        hm(s) = hm.getOrElse(s,0) + 1                                  // NOTE: λforward is to deal with recursive defs
      for (s <- syms(n) if df.contains(s))
        succ(s) = n.n::succ.getOrElse(s,Nil)
      blocks(n).foreach(hmi ++= _.used) // block results count as inner
    }                                   // syms(n) -- directSyms(n)

    for (n <- inner) hmi ++= hardSyms(n)

    // NOTE: Recursive lambdas cannot be inlined. To ensure this
    // behavior, we count λforward as additional ref to the lambda
    // in the _current_ scope. There must be at least one non-recursive
    // ref to the lambda: if it is also in the current scope the hm
    // count reaches 2, if it is in an inner scope it is accounted
    // for in hmi. Either case will prevent inlining.
    // An alternative would be to count λforward in hmi.

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

    val revNs = ns.reverse
    lastNode = revNs.headOption // not
    for (n <- revNs) {
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
      g.reflect(s1, "λ", transform(b))()
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
        g.reflectEffect(op,args:_*)(es.rkeys.map(transform).toSeq:_*)(es.wkeys.map(transform).toSeq:_*)
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
    Graph(g.globalDefs,block, g.globalDefsCache.toMap)
  }

}

abstract class CPSTransformer extends Transformer {

  val forwardMap = mutable.Map[Sym, Sym]()  // this Map set up connection for lambda-forward node (sTo -> sFrom)
  val forwardCPSSet = mutable.Set[Exp]()    // this Set collect sFrom, whose sTo has CPS effect
  val contSet = mutable.Set.empty[Exp]      // this Set collect all continuation captured by shift1 (so that their application doesn't take more continuations)

  def withSubst(s: Sym)(e: => Exp) = { subst(s) = e; subst(s) }  // syntactic helper
  def withSubstScope(args: Sym*)(actuals: Exp*)(k: => Exp) = {
    args foreach { arg => if (subst contains arg) println(s"Warning: already have a subst for $arg") }
    try {
      args.zip(actuals).foreach{ case (arg, e) => subst(arg) = e}; k
    } finally args.foreach{ arg => subst -= arg }
  }

  def traverse(y: Block, extra: Sym*)(k: Exp => Exp): Exp =
    focus0(y, extra: _*) { (path1, inner1, outer1, y) =>
      withScope1(path1, inner1) { (path0, inner0) =>
        traverse(outer1, y){ v => withScope(path0, inner0)(k(v)) }
      }
    }

  def traverse(ns: Seq[Node], y: Block)(k: Exp => Exp): Exp = {
    if (!ns.isEmpty) traverse(ns.head)(traverse(ns.tail, y)(k)) else k(transform(y.res))
  }

  def transform(b: Block)(k: Exp => Exp): Block =
    g.reify(b.in.length, (es: List[Exp]) =>
      withSubstScope(b.in:_*)(es:_*){
        traverse(b)(k)
      })

  // need to add additional input to the block, XXX CAN SIMPLIFY ?
  def transformLambda(b: Block): Block = {
    val c = Sym(g.fresh)
    val block = transform(b)(v => g.reflectWrite("@",c,v)(Adapter.CTRL))
    Block(c::block.in, block.res, block.ein, block.eff)
  }

  def reflectHelper(es: EffectSummary, op: String, args: Def*): Exp =
    if (es.deps.nonEmpty)
      g.reflectEffect(op, args: _*)(es.rkeys.map(transform).toSeq:_*)(es.wkeys.map(transform).toSeq:_*)
    else
      g.reflect(op, args:_*)

  def traverse(n: Node)(k: => Exp): Exp = n match {

    case n @ Node(s,"shift1",List(y:Block),es) =>
      contSet += y.in.head
      subst(y.in.head) = g.reflectEffect("λ", g.reify(e => withSubstScope(s)(e)(k)))()()
      traverse(y)(v => v)

    case n @ Node(s,"reset1",List(y:Block),_) =>
      subst(s) = g.reflectWrite("reset0", transform(y)(v => v))(Adapter.CTRL)
      k

    case Node(s,"λ", List(b: Block),es) =>
      if (subst contains s) { // "subst of $s has be handled by lambda forward to be ${subst(s)}"
        if (b.eff.keys contains Adapter.CPS) forwardCPSSet += forwardMap(subst(s).asInstanceOf[Sym])
        val s1: Sym = subst(s).asInstanceOf[Sym]
        g.reflect(s1, "λ", transformLambda(b))(hardSummary(forwardMap(s1)))
      } else {
        subst(s) = g.reflect("λ", transformLambda(b))
      }
      k

    case Node(f,"?",c::(a:Block)::(b:Block)::_,es) =>
      val sIf = g.reflectWrite("λ", g.reify(e => withSubstScope(f)(e)(k)))(Adapter.CTRL) // XXX without this Effect, If branch is repeated!!
      val kIf = (v:Exp) => g.reflectWrite("@",sIf,v)(Adapter.CTRL)
      withSubst(f) {
        reflectHelper(es, "?", c match {case c: Exp => transform(c); case c => ???},
          transform(a)(kIf), transform(b)(kIf))
      }

    case Node(f,"W",(c:Block)::(b:Block)::e, es) =>
      val sLoop = Sym(g.fresh)
      g.reflect(sLoop, "λ", transform(c)(v =>
        reflectHelper(es, "?", v, transform(b)(v =>
          g.reflectWrite("@", sLoop)(Adapter.CTRL)), g.reify(k))))(writeSummary(Adapter.CTRL))
      withSubst(f)(reflectHelper(es, "@", sLoop))

    case n @ Node(s,"@",(x:Exp)::(y:Exp)::_,es) if !(contSet contains x) =>
      val cont = reflectHelper(es, "λ", g.reify{e => subst(s) = e; k})
      withSubst(s)(reflectHelper(es, "@", transform(x), cont, transform(y)))

    case Node(s,"λforward",List(y:Sym), _) =>
      assert(!(subst contains y), "should not have handled lambda yet")
      val sFrom = Sym(g.fresh); val sTo = Sym(g.fresh)
      subst(s) = sFrom; subst(y) = sTo
      forwardMap(sTo) = sFrom
      g.reflect(sFrom, "λforward", sTo)()
      k

    case Node(s,op,rs,es) =>
      subst(s) = reflectHelper(es, op, rs.map {
        case b: Block => transform(b)(v => v)
        case s: Exp => transform(s)
        case a => a
      }:_*); k
  }

  def applyExp(graph: Graph): Exp = {
    bound(graph)
    withScope(Nil, graph.nodes) {
      traverse(graph.block)(v => g.reflectWrite("exit", v)(Adapter.CTRL))
    }
  }

  override def transform(graph: Graph): Graph = {
    // XXX unfortunate code duplication, either
    // with traverser or with transform(Block)
    val block = g.reify { e =>
      assert(graph.block.in.length == 1)
      subst(graph.block.in(0)) = e
      applyExp(graph)
    }
    Graph(g.globalDefs, block, g.globalDefsCache.toMap)
  }
}

abstract class SelectiveCPSTransformer extends CPSTransformer {

  override def traverse(n: Node)(k: => Exp): Exp = n match {

    case Node(s,"shift1",List(y:Block),_) => super.traverse(n)(k)
    case Node(s,"reset1",List(y:Block),_) => super.traverse(n)(k)
    case Node(s,"λforward",List(y:Sym), _) => super.traverse(n)(k)
    case Node(f,"?",c::(a:Block)::(b:Block)::_,es) if (es.keys contains Adapter.CPS) => super.traverse(n)(k)
    case Node(f,"W",(c:Block)::(b:Block)::e, es) if (es.keys contains Adapter.CPS) => super.traverse(n)(k)
    // the es.keys of "@" node may have Adapter.CPS, if and only if the lambda has Adapter.CPS
    case Node(s,"@",(x:Exp)::(y:Exp)::_,es) if (es.keys.contains(Adapter.CPS) ||
      forwardCPSSet.contains(subst(x.asInstanceOf[Sym]))) => super.traverse(n)(k)
    // lambda need to capture the CPS effect of its body block
    case Node(s,"λ", List(b: Block),es) if (b.eff.keys contains Adapter.CPS) => super.traverse(n)(k)

    case Node(s,"λ", List(b: Block),es) =>
      if (subst contains s) { // "subst of $s has be handled by lambda forward to be ${subst(s)}"
        val s1: Sym = subst(s).asInstanceOf[Sym]
        g.reflect(s1, "λ", transform(b)(v => v))(hardSummary(forwardMap(s1)))
      } else {
        subst(s) = g.reflect("λ", transform(b)(v => v))
      }
      k

    case Node(s,op,rs,es) => // catch-all case is not calling super, but transforming everything without CPS
      subst(s) = reflectHelper(es, op, rs.map {
        case b: Block => transform(b)(v => v)
        case s: Exp => transform(s)
        case a => a
      }:_*);
      k
  }
}
