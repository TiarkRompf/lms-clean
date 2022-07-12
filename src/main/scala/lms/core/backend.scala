package lms.core


import scala.collection.{immutable, mutable}

object Backend {

  // TODO: add type info and location info

  // Expressions are first-class entities in a program
  // that can be manipulated freely. An expression is
  // either a constant or a symbolic reference.
  abstract class Exp extends Def

  case class Sym(n: Int) extends Exp {
    override def toString = s"x$n"
  }
  case class Const(x: Any) extends Exp {
    override def toString = if (x != null) x.toString else "null"
  }
  implicit val orderingExp: Ordering[Exp] = Ordering.by(e => e match {
    case Const(s) => -s.##.abs
    case Sym(n) => n
  })
  implicit val orderingSym: Ordering[Sym] = Ordering.by(_.n)

  val nextName = new mutable.HashMap[String,Int]()
  final def unstableEffect(pref: String) = { // Const(pref + nextName.updateWith(pref) { v => v.map(_ + 1) orElse Some(0) } get) // 2.13!!
    val i = nextName.getOrElseUpdate(pref, 0)
    try { Const(pref + i) } finally { nextName(pref) = i + 1 }
  }


  final def UNSAFE = unstableEffect("UNSAFE")
  val STORE = Const("STORE")
  // A list of effect keys (mutable vars, ...) and
  // dependencies (previous writes, ...)
  //
  // TOOD: this can be made more fine-grained, i.e.,
  // distinguish may/must and read/write effects,
  // soft dependencies, ...
  case class EffectSummary(sdeps: Set[Sym], hdeps: Set[Sym], rkeys: Set[Exp], wkeys: Set[Exp]) {
    lazy val keys = rkeys ++ wkeys
    lazy val deps = sdeps ++ hdeps
    // the string representation of EffectSummary is "[key : sdeps | hdeps]" where write-keys have *.
    def repr(f: Exp => String) = {
      val keyString = (if (rkeys.nonEmpty) rkeys.toSeq.sorted.map(f).mkString(" ") + " " else "") +
        (if (wkeys.nonEmpty) wkeys.toSeq.sorted.map(f(_) + "*").mkString(" ") else "")
      val depString = (if (sdeps.nonEmpty) sdeps.toSeq.sorted.map(f).mkString(", ") else "_") + " | " +
        (if (hdeps.nonEmpty) hdeps.toSeq.sorted.map(f).mkString(", ") else "_")
      "[" + keyString + ": " + depString + "]"
    }
    override def toString = if (!isPure) {
      repr(_.toString)
    } else ""

    def isEmpty = isPure
    lazy val isPure = sdeps.isEmpty && hdeps.isEmpty && rkeys.isEmpty && wkeys.isEmpty
    lazy val hasSimpleEffect = wkeys.exists(key => key.isInstanceOf[Const] && key != STORE)

    def filter(f: Sym => Boolean) = {
      val nf = (e: Exp) => e match {
        case s: Sym => f(s)
        case _ => true
      }
      EffectSummary(sdeps.filter(nf), hdeps.filter(nf), rkeys.filter(nf), wkeys.filter(nf))
    }
  }

  final val emptySummary = EffectSummary(Set(), Set(), Set(), Set())
  final def softSummary(s: Sym) = EffectSummary(Set(s), Set(), Set(), Set())
  final def hardSummary(s: Sym) = EffectSummary(Set(), Set(s), Set(), Set())
  final def writeSummary(s: Exp) = EffectSummary(Set(), Set(), Set(), Set(s))

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
    + Block should separate out input effects, too
    - Should (can?) we get rid of type Def? what prevents
      us from allowing any value as part of rhs
      (guess: typeclass to/from conversion for FrontEnd)
  */

  // A definition is part of the right-hand side of a
  // node definition.
  abstract class Def

  // inputs, result, effect input, effect output summary
  case class Block(in: List[Sym], res: Exp, ein: Sym, eff: EffectSummary) extends Def {
    def bound = ein::in
    def used = res match {
      case res: Sym => eff.hdeps + res
      case _ => eff.hdeps
    }
    def deps = used ++ eff.sdeps
    // NOTE/TODO: want to mask out purely local effects
    // STORE pure??
    def isPure = eff.hdeps == Set(ein)
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

  def syms(x: Node): List[Sym] = {
    x.rhs.flatMap {
      case s: Sym => List(s)
      case b: Block => b.deps
      case _ => Nil
    } ++ x.eff.deps
  }

  def hardSyms(x: Node): Set[Sym] = {
    x.rhs.foldLeft(Set[Sym]()) {
      case (agg, s: Sym) => agg + s
      case (agg, b: Block) => agg ++ b.used
      case (agg, _) => agg
    } ++ x.eff.hdeps
  }

  case class BlockEffect(var map: Map[Exp,(Sym, List[Sym])], prev: BlockEffect) {
    var allEff: Map[Exp, List[(Sym, List[Sym])]] = Map()
    def get(key: Exp): Option[(Sym, List[Sym])] = if (prev != null) map.get(key) orElse prev.get(key) else map.get(key)
    def getOrElse(key: Exp, default: (Sym, List[Sym])) = get(key).getOrElse(default)
    def +=(kv: (Exp, (Sym, List[Sym]))) = {
      allEff = allEff + (kv._1 -> (kv._2::(allEff.getOrElse(kv._1, List()))))
      map += kv
    }
  }

}

import Backend._

class GraphBuilder {
  val globalDefs = new mutable.ArrayBuffer[Node]
  val globalDefsCache = new mutable.HashMap[Sym,Node]
  val globalDefsReverseCache = new mutable.HashMap[(String,Seq[Def]),Node]

  var nSyms = 0
  def fresh = try nSyms finally nSyms += 1

  object Def {
    def unapply(xs: Def): Option[(String,List[Def])] = xs match {
      case s @ Sym(n) =>
        findDefinition(s).map(n => (n.op,n.rhs))
      case _ => None
    }
  }

  def findDefinition(s: Exp): Option[Node] = s match {
    case s: Sym => globalDefsCache.get(s)
    case _ => None
  }
  def findDefinition(op: String, as: Seq[Def]): Option[Node] = globalDefsReverseCache.get((op,as))

  var __rewrites: (String => Any => Option[Exp]) = (s => x => None)

  def rewrite(s: String, as: List[Def]): Option[Exp] = __rewrites("")((s, as))

  def addRewrite(f: PartialFunction[Any, Option[Exp]]) = {
    val old = __rewrites
    val f1 = ((x: Any) => if (f.isDefinedAt(x)) f(x) else None)
    __rewrites = (s =>x => f1(x).orElse(old(s)(x)))
  }

  def reflect(s: String, as: Def*): Exp = {
    reflectEffect(s, as:_*)()()
  }

  def reflectRead(s: String, as: Def*)(efKeys: Exp*) = reflectEffect(s, as:_*)(efKeys: _*)()
  def reflectWrite(s: String, as: Def*)(efKeys: Exp*) = reflectEffect(s, as:_*)()(efKeys: _*)
  def reflectMutable(s: String, as: Def*) = reflectEffect(s, as:_*)(STORE)()

  // FIXME: issues:
  //  - write to STORE doens't really capture the meaning of free
  //  - if one of the free is DCE, the "path" through write on store is broken thus free aren't generated:
  //      x11: free(x1) // deps ...
  //      x12: free(x2) // hdeps x11
  //      x13: free(x3) // hdeps x12
  //
  //    x12 DCEed...
  //      x11: free(x1) // deps ...  <- non reachable
  //      x13: free(x3) // hdeps x12
  def reflectFree(s: String, as: Def*)(efKeys: Exp) = reflectEffect(s, as:_*)()(efKeys, STORE)
  def reflectRealloc(s: String, as: Def*)(efKeys: Exp) = reflectEffect(s, as:_*)(STORE)(efKeys, STORE)

  def reflectUnsafe(s: String, as: Def*) = reflectEffect(s, as:_*)(UNSAFE)()


  def latest(e1: Exp) = if (curLocalDefs(e1)) e1.asInstanceOf[Sym] else curBlock
  def getLastWrite(x: Exp) = curEffects.get(x) match {
    case Some((lw, _)) => lw
    case _ => latest(x)
  }

  var reflectHere = false
  def reflectEffectSummary(s: String, as: Def*)(efKeys: (Set[Exp], Set[Exp])): Exp =
    reflectEffect(s, as:_*)(efKeys._1.toSeq:_*)(efKeys._2.toSeq:_*)
  def reflectEffectSummaryHere(s: String, as: Def*)(efKeys: (Set[Exp], Set[Exp])): Exp =
    scopedReflectHere(true)(reflectEffectSummary(s, as:_*)(efKeys))
  def scopedReflectHere(flag: Boolean)(closure: => Exp): Exp = {
    val saveReflectHere = reflectHere
    reflectHere = flag
    try {
      closure
    } finally { reflectHere = saveReflectHere }
  }

  def isCurrentValue(src: Exp, value: Sym) = !curEffects.get(src).filter({ case (_, lrs) => lrs contains value }).isEmpty
  // This is the main function for reflect with explicit read/write effects
  def reflectEffect(sm: Sym, s: String, as: Def*)(readEfKeys: Exp*)(writeEfKeys: Exp*): Exp = {
    // simple pre-construction optimization
    rewrite(s, as.toList) match {
      case Some(e) => e // found optimization (resulting in pure expressions only)
      case None => // no available optimization
        // latent effects? closures, mutable vars, ... (these are the keys!)
        val (latent_ref, latent_wef) = getLatentEffect(s, as:_*)
        val (reads, _writes) = ((latent_ref ++ readEfKeys).toSet, (latent_wef ++ writeEfKeys).toSet)
        // FIXME(feiw) get this special case refactored.
        val writes = if (s == "reset1") _writes filter (_ != stub.Adapter.CPS) else _writes

        if (reads.nonEmpty || writes.nonEmpty) {
          val (prevHard, prevSoft) = gatherEffectDeps(reads, writes, s, as:_*)
          // prevSoft --= prevHard
          val summary = EffectSummary(prevSoft, prevHard, reads, writes)
          val res = reflect(sm, s, as:_*)(summary)
          // update effect environments (curEffects, curLocalReads, and curLocalWrites)
          curLocalReads ++= reads
          curLocalWrites ++= writes
          for (key <- reads) {
            val (lw, lrs) = curEffects.getOrElse(key, (curBlock, Nil)) // FIXME really?
            curEffects += key -> (lw, res::lrs)
            if (key == STORE) curEffects += res -> (res, Nil) // Needed to notify res was defined locally
          }                                                            // Do we want it?
          for (key <- writes) { curEffects += key -> (res, Nil) }
          res
        } else {
          // We can run Common Subexpression Elimination (CSE) for pure nodes
          findDefinition(s,as) match {
            case Some(n) => n.n
            case None =>
              reflect(sm, s, as:_*)()
          }
        }
    }
  }
  def reflectEffect(s: String, as: Def*)(readEfKeys: Exp*)(writeEfKeys: Exp*): Exp = {
    reflectEffect(Sym(fresh), s, as:_*)(readEfKeys:_*)(writeEfKeys:_*)
  }

  def gatherEffectDeps(reads: Set[Exp], writes: Set[Exp], s: String, as: Def*): (Set[Sym], Set[Sym]) = {
    val (prevHard, prevSoft) = (new mutable.ListBuffer[Sym], new mutable.ListBuffer[Sym])
    // gather effect dependencies 1): handle the write keys
    for (key <- writes) {
      curEffects.get(key) match {
        case Some((lw, lr)) =>
          val (sdeps, hdeps) = gatherEffectDepsWrite(s, as, lw, lr)
          prevSoft ++= sdeps; prevHard ++= hdeps
        case _ =>
          // write has hard dependencies on declaration (if declared locally) or block (if declared globally, i.e., out of current block)
          prevHard += latest(key);
      }
    }
    // gather effect dependencies 2): handling of reifyHere
    // reifyHere is an Effect Optimization for conditionals (if and switch)
    // it allows the block of conditionals to be aware of the `curEffects` out of the block
    // The exact demonstration of the optimization is in test IfDCETest "if_effect_reifyHere".
    if (reifyHere) prevHard += curBlock
    // gather effect dependencies 3): handle read keys (i.e., reads have hard dependencies on previous write)
    for (key <- reads) {
      prevHard += getLastWrite(key)
    }
    (prevHard.toSet, prevSoft.toSet)
  }

  // Conservative handling of dependencies for write keys: (return soft deps and hard deps respectively)
  // 1) write has hard dependencies on previous write (this is conservative for array case, Store, Ctrl,...)
  // 2) write has soft dependencies on previous read (just enforcing order, do not enforcing the reads to be scheduled)
  def gatherEffectDepsWrite(s: String, as: Seq[Def], lw: Sym, lr: Seq[Sym]): (Set[Sym], Set[Sym]) =
    (if (!reflectHere) lr.toSet else Set(), Set(latest(lw))) // FIXME(feiw) why not adding soft dependencies when reflectHere?

  // FIXME: take EffectSummary as argument?
  def reflect(x: Sym, s: String, as: Def*)(summary: EffectSummary = emptySummary): Sym = {
    val n = Node(x, s, as.toList, summary)
    globalDefs += n
    globalDefsCache(x) = n
    globalDefsReverseCache((s,n.rhs)) = n
    curLocalDefs += x
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

  // getLatentEffect(x: Def) collect readkeys and writekeys of a Def (which can be a Block or an Exp)
  // If we see a Block, then it might be a block of conditionals or loops (or other IR constructs that hold blocks).
  // If we see a Sym that is a function (lambda), at first glance, we should not extract the effects of the function
  //    because function definition has NO effects. However, the IR constructs might be applying the function without
  //    using the explicit @ IR construct. In that case, we conservatively analyze the latent effect of the function.
  //    One such example is `forloop(3, x => x + 1)`, where the lambda IS applied.
  // This function is only called from getLatentEffect(xs: Def*).
  def getLatentEffect(x: Def): (Set[Exp], Set[Exp]) = x match {
    case b: Block =>
      getEffKeys(b)
    case s: Sym =>
      findDefinition(s) match {
        case Some(Node(_, "λ", (b@Block(ins, out, ein, eout))::_, _)) =>
          // 1. At first glance, this lambda case seems redundant, because we know function definition should
          //    not impose effects (only function application can)
          //    However, in some cases, function application is done with syntax other than `@`
          //    for instance: forloop(3, x => x + 1), where `forloop` is a target langange construct that `applies`
          //    the lambda 3 times. These flexible target language constructs allows functions to be applied in a
          //    non-@ syntax, thus complicating the effect computation here.
          // 2. FIXME(feiw):
          //    You can see that we are not getting the effect for lambda arguments here, because we have an (unchecked)
          //    assumption that the functions in these target langauge constructs do not have effects on parameters.
          //    If they do, we are not sure how to get the arguments, so to propagate the effects from parameters to arguments.
          getEffKeys(b)
        case _ =>
          // FIXME(feiw):
          // In fact, it appears to have errors here, since lambdas can be wrapped in other structs (such as conditionals)
          (Set[Exp](), Set[Exp]())
      }
    case _ =>
      (Set[Exp](), Set[Exp]())
  }

  // getLatentEffect(xs: Def*) wrappers getLatentEffect(x: Def) and accumulate effects of multiple Defs
  def getLatentEffect(xs: Def*): (Set[Exp], Set[Exp]) =
    xs.foldLeft((Set[Exp](), Set[Exp]())) { case ((r, w), x) =>
      val (ref, wef) = getLatentEffect(x)
      (r ++ ref, w ++ wef)
    }

  // getFunctionLatentEffect is for getting latent effects for functions.
  // It takes an Exp, which should be evaluated to a lambda (or lambda forward).
  // It returns ((read_keys, write_keys), (read_parameters, write_parameters), result)
  //              Set[Exp]   Set[Exp]      Set[Int]: index  Set[Int]: index    Option[Exp]
  //   1. read_keys/write_keys: effect keys of the function (excluding effects to parameters)
  //   2. read_parameters/write_parameters: effects of the function to its parameters (just returning indices, not Exps)
  //      Using Set[Int] (index) for read_parameters and write_parameters is necessary because in the
  //        conditional case, the parameters may have different names (symbols) and cannot be Unioned.
  //        the indices can be unioned easily
  //   3. result: the result of the function body (useful if the result is another function that has latent effects)
  //        it is Option[Exp] because in some cases I am not sure what result to return.
  // FIXME(feiw) Dig further to see if/why lambda_forward or None cases are correct
  // FIXME(feiw) in the conditional case, the handling of result is still wrong.
  def getFunctionLatentEffect(f: Exp): ((Set[Exp], Set[Exp]),(Set[Int], Set[Int]), Option[Exp]) = findDefinition(f) match {
      case Some(Node(_, "λ", (b:Block)::_, _)) =>
        getEffKeysWithParam(b)
      case Some(Node(_, "λforward", xf::Const(arity:Int)::Nil, _)) =>
        // for lambdaforward, there are several options:
        // 1. take the effect of `xf`. However, this is very tricky since `xf` node is not yet constructed at this moment
        //    (maybe block of the `xf` function is not yet reified), and the application of lambda-forward might just be part of that block
        // 2. stop the world effect, which is safe. FIXME(feiw): how to implement it?
        // 3. temp solution: add read write effects to all arguments.
        // what about doubly recursive?
        ((Set[Exp](), Set[Exp](Const("CTRL"))), (0.until(arity).toSet, 0.until(arity).toSet), None)
      case None => // FIXME: function argument? fac-01 test used for recursive function...
        ((Set[Exp](), Set[Exp](Const("CTRL"))), (Set[Int](), Set[Int]()), None)
      case Some(Node(_, "@", (f: Sym)+:args, _)) =>
        val ((rk, wk), Some(f_res)) = getApplyLatentEffect(f, args: _*)
        val ((rk2, wk2), (prk2, pwk2), f_res_res) = getFunctionLatentEffect(f_res)
        ((rk ++ rk2, wk ++ wk2), (prk2, pwk2), f_res_res)
      case Some(Node(_, "?", c::Block(ins, out, ein, eout)::Block(ins2, out2, ein2, eout2)::Nil, _)) =>
        val ((rk, wk), (prk, pwk), _) = getFunctionLatentEffect(out)
        val ((rk2, wk2), (prk2, pwk2), _) = getFunctionLatentEffect(out2)
        ((rk ++ rk2, wk ++ wk2), (prk ++ prk2, pwk ++ pwk2), None) // FIXME(feiw)
      case Some(Node(_, "module", Const(manno)::(b:Block)::_, _)) =>
        getEffKeysWithParam(b)
      case Some(e) => throw new Exception(s"not yet handling node $e in getFunctionLatentEffect")
        // FIXME what about @, ?, array_apply => conservative write on all args?
        // Cleary the current solution is not complete and needs to be extended for more constructs or re-do in a different manner:
        // Effects: 1. overlay types on variables
        //          2. be conservative (with stop-the-world)
        // Aliasing: 1. track precisesly
        //           2. (like rust) cannot alias mutable variables (onwership tracking)
        // Regions: (chat with Yuyan)
  }

  // getApplyLatentEffect(f: Sym, args: Def*) is for getting latent effects for function application:
  // 1. get the lambda latent effects from the function
  // 2. update the lambda latent effects (replace parameters with arguments)
  // The sub functon also returns the result of the function because the result might
  //    be another function that is applied (we need to get its latent effects).
  def getApplyLatentEffect(f: Sym, args: Def*): ((Set[Exp], Set[Exp]), Option[Exp]) = {

    // Just collecting the latent effects of arguments
    val (reads, writes) = getLatentEffect(args: _*)

    // the freads/fwrites are read/write keys of the function (excluding parameters)
    // the preads/pwrites are read/write keys of the function parameters (they are Set[Int] as indices, rather than Set[Exp])
    // the res is the result of the function body. It is needed because the result of the body can be another function that
    //     we need to get the latent effects of.
    val ((freads, fwrites), (preads, pwrites), res) = getFunctionLatentEffect(f)

    // For @ we need to replace the effect on parameters to the actual arguments.
    // the asInstanceOf seems unsafe at first glance. However, it is not a problem since a standalone block
    // should never be an argument in function application.
    ((reads ++ freads ++ preads.map(args(_).asInstanceOf[Exp]), writes ++ fwrites ++ pwrites.map(args(_).asInstanceOf[Exp])), res)
  }

  // The main function for getting latent effects
  // Latent effects are effects in Blocks (conditional, loop, lambda)
  def getLatentEffect(op: String, xs: Def*): (Set[Exp], Set[Exp]) = (op, xs) match {
    case ("λ", _) => (Set[Exp](), Set[Exp]())
    case ("@", (f: Sym)+:args) => getApplyLatentEffect(f, args:_*)._1
    case _ => getLatentEffect(xs:_*)
  }


  var curBlock: Sym = _ // could this become an ExplodedStruct?
  var curEffects: BlockEffect = _ // map key to write/read deps, was read?
  var curLocalDefs: Set[Exp] = _
  var curLocalReads: Set[Exp] = _
  var curLocalWrites: Set[Exp] = _
  var reifyHere: Boolean = false

  // NOTE/TODO: want to mask out purely local effects

  def getInnerNodes(b: Block): List[Node] = {
    val bound = new Bound
    bound(Graph(globalDefs.toList,b, globalDefsCache.toMap))
    val ins = b.ein::b.in
    globalDefs.toList.filter(n => ins.exists(bound.hm.getOrElse(n.n,Set())))
  }

  def getEffKeys(b: Block) = (b.eff.rkeys, b.eff.wkeys)
  def mergeEffKeys(b: Block, c: Block) =
    (b.eff.rkeys ++ c.eff.rkeys, b.eff.wkeys ++ c.eff.wkeys)
  def splitParamKeys(keys: Set[Exp], ins: List[Sym]): (Set[Exp], Set[Int]) = {
    val (params, keysWithOutParams) = keys.partition(ins.contains(_))
    (keysWithOutParams, params.map(ins.indexOf(_)))
  }
  def getEffKeysWithParam(b: Block) = {
    val (brkeys, prkeys) = splitParamKeys(b.eff.rkeys, b.in)
    val (bwkeys, pwkeys) = splitParamKeys(b.eff.wkeys, b.in)
    ((brkeys, bwkeys), (prkeys, pwkeys), Some(b.res))
  }

  def reify(f: => Exp): Block =  reify(0, _ => f)
  def reifyHere(f: => Exp): Block =  reify(0, xs => f, true)
  def reify(f: Exp => Exp): Block = reify(1, xs => f(xs(0)))
  def reifyHere(f: Exp => Exp): Block =  reify(1, xs => f(xs(0)), true)
  def reify(f: (Exp, Exp) => Exp): Block = reify(2, xs => f(xs(0), xs(1)))
  def reify(f: (Exp, Exp, Exp) => Exp): Block = reify(3, xs => f(xs(0), xs(1), xs(2)))
  def reify(f: (Exp, Exp, Exp, Exp) => Exp): Block = reify(4, xs => f(xs(0), xs(1), xs(2), xs(3)))

  def withBlockScopedEnv(here: Boolean)(closure: => Block): Block = {
    val save = curBlock
    val saveEffects = curEffects
    val saveLocalDefs = curLocalDefs
    val saveLocalReads = curLocalReads
    val saveLocalWrites = curLocalWrites
    val saveReifyHere = reifyHere
    try {
      curBlock = Sym(fresh)
      curEffects = BlockEffect(Map(), if (here) curEffects else null)
      reifyHere = here
      curLocalDefs = Set()
      curLocalReads = Set()
      curLocalWrites = Set()
      closure
    } finally {
      curBlock = save
      curEffects = saveEffects
      curLocalDefs = saveLocalDefs
      curLocalReads = saveLocalReads
      curLocalWrites = saveLocalWrites
      reifyHere = saveReifyHere
    }
  }

  def reify(arity: Int, f: List[Exp] => Exp, here: Boolean = false): Block = withBlockScopedEnv(here){
    val args = (0 until arity).toList.map(_ => Sym(fresh))
    val res = f(args)
    // remove local definitions from visible effect keys
    val reads = curLocalReads.filterNot(curLocalDefs)
    val writes = curLocalWrites.filterNot(curLocalDefs)
    // TODO: it is possible to remove the dependencies, too (--> DCE for var_set / need to investigate more)
    // for (e <- curEffects.keys if curLocalDefs(e)) curEffects -= e
    // TODO:
    //  - if tests
    //  - while tests
    //  - closure test
    var hard = writes.map(curEffects.map(_)._1)
    if (curEffects.map.contains(res)) // if res is a local mutable (e.g. Array)
      hard += curEffects.map(res)._1
    if (hard.isEmpty)
      hard = Set(curBlock)

    Block(args, res, curBlock, EffectSummary(Set(), hard, reads, writes))
  }
}

class GraphBuilderOpt extends GraphBuilder {

  // fine grained dependency computation for array
  override def gatherEffectDepsWrite(s: String, as: Seq[Def], lw: Sym, lr: Seq[Sym]): (Set[Sym], Set[Sym]) =
    findDefinition(latest(lw)) match {
      case Some(Node(_, "array_set", as2, deps)) if (s == "array_set" && as.init == as2.init) =>
        // If latest(lw) is setting the same array at the same index, we do not add hard dependence but soft dependence
        // In addition, we need to inherite the soft and hard deps of latest(lw)
        (deps.sdeps + latest(lw), deps.hdeps)
      case _ => super.gatherEffectDepsWrite(s, as, lw, lr)
    }

  // graph pre-node-construction optimization
  addRewrite {
    // staticData(as)(i) => staticData(as(i))
    case ("array_get", List(Def("staticData", List(Const(as: Array[_]))), Const(i:Int))) =>
      as(i) match {
        case a: Int => Some(Const(a))
        case a => Some(reflect("staticData", Const(a)))
      }

    // FIXME: Can we generalize that for mutable objects?
    // as(i) = as(i) => ()   side condition: no write in-between!
    case ("array_set", List(as:Exp, i, rs @ Def("array_get", List(as1: Exp, i1))))
      if as == as1 && i == i1 && {
        // rs is part of the list of read since the last write
        curEffects.get(as).filter({ case (_, lrs) => lrs contains rs.asInstanceOf[Sym] }).isDefined } =>
      Some(Const(()))

    // as(i) = rs; ...; as(i) = rs => as(i) = rs; ...; () side condition no write in-between
    case ("array_set", List(as: Exp, i, rs)) if ({curEffects.get(as).flatMap({ case (lw, _) => findDefinition(lw)}) match {
        case Some(Node(_, "array_set", List(as2, idx2, value2), _)) if (as == as2 && i == idx2 && value2 == rs) => true
        case _ => false
      }
    }) => Some(Const(()))

    // TODO: should be handle by the case below. However it doesn't because of aliasing issues!
    // (x + idx)->i = (x + idx)->i => ()    side condition: no write in-between! (on x)
    case ("reffield_set", List(Def("array_slice", (as: Exp)::idx::_), i, rs @ Def("reffield_get", List(Def("array_slice", (as1: Exp)::idx1::_), i1)))) =>
      // System.out.println(s">>> $as + $idx -> $i == $as1 + $idx1 -> $i1")
      if (as == as1 && idx == idx1 && i == i1 && {
          // rs is part of the list of read since the last write
          // System.out.println(s">>> ${curEffects.get(as)}")
          // System.out.println(s"  |->>> $as + $idx -> $i == $as1 + $idx1 -> $i1")
          curEffects.get(as).filter({ case (_, lrs) => lrs contains rs.asInstanceOf[Sym] }).isDefined })
        Some(Const(()))
      else None

    // x-> i = x->i => ()    side condition: no write in-between!
    case ("reffield_set", List(as:Exp, i, rs @ Def("reffield_get", List(as1: Exp, i1)))) =>
      // System.out.println(s">>> $as -> $i == $as1 -> $i1")
      if (as == as1 && i == i1 && {
          // rs is part of the list of read since the last write
          curEffects.get(as).filter({ case (_, lrs) => lrs contains rs.asInstanceOf[Sym] }).isDefined })
        Some(Const(()))
      else
        None

    // x = x => ()    side condition: no write in-between!
    case ("var_set", List(as:Exp, rs @ Def("var_get", List(as1: Exp)))) =>
      // System.out.println(s">>> $as -> $i == $as1 -> $i1")
      if (as == as1 && {
          // rs is part of the list of read since the last write
          curEffects.get(as).filter({ case (_, lrs) => lrs contains rs.asInstanceOf[Sym] }).isDefined })
        Some(Const(()))
      else
        None

    // [var] x = y; ....; x => [var] x = y; ....; y    side condition: no write in-between!
    case ("var_get", List(as:Exp)) =>
      curEffects.get(as).flatMap({ case (lw, _) => findDefinition(lw) collect {
        case Node(_, "var_new", List(init: Exp), _) => init
        case Node(_, "var_set", List(_, value: Exp), _) => value
      }})

    // x[i] = y; ....; x[i] => x[i] = y; ....; y    side condition: no write in-between!
    case ("array_get", List(as:Exp,i:Exp)) =>
      curEffects.get(as).flatMap({ case (lw, _) => findDefinition(lw) collect {
        case Node(_, "array_set", List(_, i2: Exp, value: Exp), _) if i == i2 => value
      }})

    case ("array_slice", List(as: Exp, Const(0), Const(-1))) => Some(as)
    case ("array_length", List(Def("NewArray", Const(n)::_))) =>
      Some(Const(n))
    case ("array_length", List(Def("Array", as))) =>
      Some(Const(as.length))
    case ("array_length", List(Def("array_slice", List(x, s, e)))) =>
      Some(reflect("-", e, s))

    case ("String.length", List(Const(as: String))) =>
      Some(Const(as.length))
    case ("String.charAt", List(Const(as: String), Const(idx: Int))) =>
      Some(Const(as.charAt(idx)))

    case ("!", List(Const(b: Boolean))) => Some(Const(!b))
    case ("==", List(Const(a: Double), _)) if a.isNaN => Some(Const(false))
    case ("==", List(_, Const(b: Double))) if b.isNaN => Some(Const(false))
    case ("==", List(Const(a), Const(b))) => Some(Const(a == b))
    case ("!=", List(Const(a: Double), _)) if a.isNaN => Some(Const(true))
    case ("!=", List(_, Const(b: Double))) if b.isNaN => Some(Const(true))
    case ("!=", List(Const(a), Const(b))) => Some(Const(a != b))
    case ("^", List(Const(a: Boolean), Const(b: Boolean))) => Some(Const(a ^ b))
    case ("<=", List(Const(a: Int), Const(b: Int))) => Some(Const(a <= b))
    case ("<=", List(Const(a: Float), Const(b: Float))) => Some(Const(a <= b))
    case ("<=", List(Const(a: Long), Const(b: Long))) => Some(Const(a <= b))
    case ("<=", List(Const(a: Double), Const(b: Double))) => Some(Const(a <= b))
    // idea 1:
    // case ("<=", List(Const(a), Const(b))) if isNum(a) => Some(Const(a.asInstanceOf[Double] <= b.asInstanceOf[Double]))
    // idea 2:
    //   implicit val m: Manifest[Int] = typeMap(Const(a)).asInstanceOf[Manifest[Int]]
    //   val tmp = num[Int](m).lteq(wrap[Int](a), wrap[Int](b))
    //   Some(Const(tmp))
    // }
    case (">=", List(Const(a: Int), Const(b: Int))) => Some(Const(a >= b))
    case (">=", List(Const(a: Long), Const(b: Long))) => Some(Const(a >= b))
    case (">=", List(Const(a: Float), Const(b: Float))) => Some(Const(a >= b))
    case (">=", List(Const(a: Double), Const(b: Double))) => Some(Const(a >= b))
    case ("<", List(Const(a: Int), Const(b: Int))) => Some(Const(a < b))
    case ("<", List(Const(a: Long), Const(b: Long))) => Some(Const(a < b))
    case ("<", List(Const(a: Float), Const(b: Float))) => Some(Const(a < b))
    case ("<", List(Const(a: Double), Const(b: Double))) => Some(Const(a < b))
    case (">", List(Const(a: Int), Const(b: Int))) => Some(Const(a > b))
    case (">", List(Const(a: Long), Const(b: Long))) => Some(Const(a > b))
    case (">", List(Const(a: Float), Const(b: Float))) => Some(Const(a > b))
    case (">", List(Const(a: Double), Const(b: Double))) => Some(Const(a > b))

    case ("?", c::(t: Block)::(e: Block)::_) if t.isPure && e.isPure && t.res == e.res => Some(t.res)
    case ("?", (c: Sym)::(t: Block)::(e: Block)::_) if t.isPure && e.isPure => (t.res, e.res) match {
      case (Const(t: Double), Const(e: Double)) if t.isNaN && e.isNaN => Some(Const(Double.NaN))
      // c && true or c || false => if (c) true else false
      // if (c) false else true
      case (Const(t: Boolean), Const(e: Boolean)) /* if t != e */ => Some(if (t) c else reflect("!", c))
      case _ => None
    }
    //case _  =>
    //  super.rewrite(s,as)
  }

  // From miniscala CPSOptimizer.scala
  val leftNeutral: Set[(Any, String)] =
    Set((0, "+"), (1, "*"), (~0, "&"), (0, "|"), (0, "^"))
  val rightNeutral: Set[(String, Any)] =
      Set(("+", 0), ("-", 0), ("*", 1), ("/", 1),
          ("<<", 0), (">>", 0), (">>>", 0),
          ("&", ~0), ("|", 0), ("^", 0))
  val leftAbsorbing: Set[(Any, String)] =
    Set((0, "*"), (0, "&"), (~0, "|"))
  val rightAbsorbing: Set[(String, Any)] =
    Set(("*", 0), ("&", 0), ("|", ~0))

  val sameArgReduce: Map[String, Any] =
    Map("-" -> 0, "/" -> 1, "%" -> 0, "^" -> 0,
      "<=" -> true, ">=" -> true, "==" -> true,
      "<" -> false, ">" -> false, "!=" -> false)

  // graph pre-node-construction optimization
  override def reflect(s: String, as: Def*): Exp = (s,as.toList) match {
    case ("+", List(Const(a:Int),Const(b:Int))) => Const(a+b)
    case ("-", List(Const(a:Int),Const(b:Int))) => Const(a-b)
    case ("*", List(Const(a:Int),Const(b:Int))) => Const(a*b)
    case ("/", List(Const(a:Int),Const(b:Int))) => Const(a/b)

    case ("+", List(Const(a:Long),Const(b:Long))) => Const(a+b)
    case ("-", List(Const(a:Long),Const(b:Long))) => Const(a-b)
    case ("*", List(Const(a:Long),Const(b:Long))) => Const(a*b)
    case ("/", List(Const(a:Long),Const(b:Long))) => Const(a/b)

    case ("+", List(Const(a:Double),Const(b:Double))) => Const(a+b)
    case ("-", List(Const(a:Double),Const(b:Double))) => Const(a-b)
    case ("*", List(Const(a:Double),Const(b:Double))) => Const(a*b)
    case ("/", List(Const(a:Double),Const(b:Double))) => Const(a/b)

    case ("%", List(Const(a:Int),Const(b:Int))) => Const(a%b)
    case (">>>", List(Const(a: Int),Const(b:Int))) => Const(a >>> b)
    case (">>>", List(Const(a: Long),Const(b:Int))) => Const(a >>> b)
    case ("<<",  List(Const(a: Int),Const(b:Int))) => Const(a << b)
    case ("<<", List(Const(a: Long),Const(b:Int))) => Const(a << b)
    case ("&", List(Const(a: Long),Const(b:Long))) => Const(a & b)
    case (op, List(Const(x),b:Exp)) if leftNeutral((x, op)) => b
    case (op, List(a:Exp,Const(x))) if rightNeutral((op, x)) => a
    case (op, List(Const(x),b:Exp)) if leftAbsorbing((x, op)) => Const(x)
    case (op, List(a:Exp,Const(x))) if rightAbsorbing((op, x)) => Const(x)
    case (op, List(a,b)) if a == b && sameArgReduce.contains(op) => Const(sameArgReduce(op))

    // TBD: can't just rewrite, need to reflect block!
    // case ("?", List(Const(true),a:Block,b:Block)) => a

    // for now we implement the front-end method as a
    // a smart constructor (might revisit later)

    case p =>
      super.reflect(s, as:_*)
  }
}

case class Graph(val nodes: Seq[Node], val block: Block, val globalDefsCache: immutable.Map[Sym,Node]) {
  // contract: nodes is sorted topologically
  def show: Unit = System.out.println(toString)

  override def toString = {
    val source = new java.io.ByteArrayOutputStream()
    val stream = new java.io.PrintStream(source)
    stream.println("=================")
    for (node <- nodes)
      stream.println(node)
    stream.println(block)
    stream.println("=================")
    source.toString
  }
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

    g.copy(nodes = g.nodes.filter(d => live(d.n)))
  }
}

/*
 * Resolve may dependencies
 */
class DeadCodeElimCG extends Phase {

  final val fixDeps = true // remove dependencies on removed nodes
  // it is interesting to discuss the difference between `live` and `reach`.
  // `reach` is the set of nodes (or Syms of nodes) that are reachable via hard-dependencies.
  // the reachable set can include values that are needed (data dependencies) and side-effects
  // (printing, mutation, et. al.)
  // `live` is the set of nodes (or Syms of nodes) whose values are needed (only data dependencies).
  // For instance, a conditional can have side-effects and return values. If the side-effects
  // are relevant, then the conditional is reachable. If the values are relevant, the conditional
  // is live. The property of `live` and `reach` can be independent.
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

    val s_live = new mutable.BitSet
    val s_reach = new mutable.BitSet
    statics = new mutable.HashSet[Node]
    var newNodes: List[Node] = Nil
    val used = new mutable.BitSet
    var size = 0

    // First pass liveness and reachability
    // Only a single pass that reduce input size and first step of the next loop
    utils.time("A_First_Path") {
      s_reach ++= g.block.used.map(_.n)
      if (g.block.res.isInstanceOf[Sym]) {
        s_live += g.block.res.asInstanceOf[Sym].n
        used += g.block.res.asInstanceOf[Sym].n
      }
      used ++= g.block.bound.map(_.n)
      for (d <- g.nodes.reverseIterator) {
        if (s_reach(d.n.n)) {
          val nn = d match {
            case n @ Node(s, "?", c::(a:Block)::(b:Block)::t, eff) if !s_live(s.n) =>
              n.copy(rhs = c::a.copy(res = Const(()))::b.copy(res = Const(()))::t) // remove result deps if dead
            case _ => d
          }
          s_live ++= valueSyms(nn).map(_.n)
          s_reach ++= hardSyms(nn).map(_.n)

          newNodes = nn::newNodes
        }
      }
    }

    live = s_live.toList.map(Sym(_)).toSet
    reach = s_reach.toList.map(Sym(_)).toSet

    val filterSym : Exp => Boolean =
      exp => exp match {
       case(Sym(_)) => true
       case _ => false
      }

    // Second pass remove unused variables
    var idx: Int = 1
    while (size != used.size) {
      utils.time(s"Extra_Path_$idx") {
        size = used.size
        for (d <- newNodes.reverseIterator) {
          if (used(d.n.n)) {
            used ++= valueSyms(d).map(_.n)
          } else if (d.eff.hasSimpleEffect || d.eff.wkeys.filter(filterSym).map(_.asInstanceOf[Sym].n).exists(used)) {
            used += d.n.n
            used ++= valueSyms(d).map(_.n)
          }
        }
      }
      idx += 1
    }

    val filterkey : Exp => Boolean =
      key => key match {
        case Sym(n) => used(n)
        case _ => true
      }

    utils.time(s"Recreate_the_graph") {
      var newGlobalDefsCache = Map[Sym,Node]()
      newNodes = for (d <- newNodes if used(d.n.n)) yield {
        newGlobalDefsCache += d.n -> d
        if (d.op == "staticData") statics += d
        if (fixDeps)
          d.copy(rhs = d.rhs.map {
            case b: Block => b.copy(eff = b.eff.filter(filterkey))
            case o => o
          }, eff = d.eff.filter(filterkey))
        else
          d
      }
      val newBlock = if (fixDeps)
        g.block.copy(eff = g.block.eff.filter(filterkey))
      else
        g.block

      Graph(newNodes, newBlock, newGlobalDefsCache)
    }
  }
}


// Compute bound structure information
// (i.e., which bound variables a node depends on)
class Bound extends Phase {

  val hm = new mutable.HashMap[Sym,Set[Sym]]

  def apply(g: Graph): Graph = {
    val bound = g.nodes.flatMap(boundSyms).toSet ++ g.block.bound

    // For recursive syms, we don't want to force
    // non-recursive deps into nested scopes
    // for (Node(b,"λ",_,_) <- g.nodes)
    //   hm(b) = Set()
    //
    // This explicit initialization was previously needed
    // since we used hm.getOrElse(a,Set(a)) as default below
    // (there is a choice whether undefined symbols should be
    // treated as bound or not -- this case is typically only
    // encountered for recursive lambda refs).

    for (b <- bound)
      hm(b) = Set(b)

    // Convergence loop: we want to make sure that recursive
    // refs via λforward nodes acquire the same bound information
    // as the lambda itself (which comes later in the list) and
    // hence get scheduled into the same block (see recursion_3 test).

    var more = true
    while (more) {
      more = false

      for (d <- g.nodes) {
        val b = boundSyms(d).toSet - d.n
        val newSyms = syms(d).flatMap(a => hm.getOrElse(a,Set())).toSet -- b
        more ||= (d.op == "λforward" && hm.get(d.n) != Some(newSyms))
        hm(d.n) = newSyms
      }
    }

    //hm.foreach(println)

    g
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
    case Node(f, "λ", Block(in, y, ein, eff)::_, _) =>
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
