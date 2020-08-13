# LMS IR

In the file lms/core/backend.scala, the core LMS IR is defined in `object Backend`.

``` scala
abstract class Def // Definition: used in right-hand-side of all nodes
abstract class Exp extends Def
case class Sym(n: Int) extends Exp   // Symbol
case class Const(x: Any) extends Exp // Constant
case class Block(in: List[Sym], res: Exp, ein: Sym, eff: EffectSummary) extends Def
case class Node(n: Sym, op: String, rhs: List[Def], eff: EffectSummary)
```

From this class hierarchy, we can see that the basic unit of the LMS IR is `Exp`,
which can be `Const(0)`, `Const("hello")`, or `Sym(2)`, which really means `x2` in the
generated code. We can combine basic units into `Node`, such as

``` scala
Node(Sym(x3), "+", List(Sym(x2), Const(1)), _)
```
the semantics of which is `x3 = x2 + 1`. As you can probably see, the node `op` is just
a string, which allows eazy extension of various kinds of nodes such as `-`, `print`,
and et al.

Using `Exp` and `Node`, we can construct program of arbitrary sizes, but there is still
no blocks or scopes in the IR, which is necessary for constructing conditionals, loops,
functions, and et al. The `case class Block` is exactly for that purpose. However, it
might be surprising that the `Block` class doesn't really contain a list of nodes that
are in the block. Instead, it has `in: List[Sym]` as the inputs of the block, `res: Exp`
as the output of the block, and then `ein: Sym` and `eff: EffectSummar` for effect-related
contents (will cover later). Where are the nodes that should go into the block?

It turned out that the nodes are in the graph, but not explicitly scoped in the block.
when traversing the graph (for analysis, transformation, or code generation), the nodes
of each block are dynamically computed from all available nodes, using the *dependencies*.
This is the feature of *sea of nodes* styled IR, which is covered in more details later.

Some may wonder why going for sea of nodes IR, where dynamically computing the nodes
of a scope seems to be an overhead for any graph traversal. It is true that dynamically
computing the nodes of a scope adds overhead. However, the advantage is that nodes are
not fixed to a certain scope, and scheduling the nodes (in or out-of scopes) can be
a powerful optimization (called code motion) of the IR. For instance, the user of LMS
might constructed a graph like this:

``` scala
for (i <- 0 until 100) {
  val a: Int = f(10) // some heavy and pure function
  print(a + i)
}
```

It would be beneficial to optimize the code to

``` scala
val a: Int = f(10) // some heavy and pure function
for (i <- 0 until 100) {
  print(a + i)
}
```

### Dependencies and Effects

From the previous example, hopefully the reader has got a basic understanding of why
we use sea of node IR. But we have not talked about how (in a high level) we schedule
nodes of a scope. We briefly mentioned "dependencies" above. What are "dependencies"?

There are 2 kinds of dependencies that we care about in LMS IR:
1. the data dependency and
2. the control dependencies.

The data dependency is easier to capture. If a node uses an `Exp`, then the node
depends on that `Exp`, and the node that generates that `Exp`. The data dependency
is also captured explicitly by the graph. For instance, in code:

``` scala
val x1 = 10 + x0
val x2 = x1 + 100
```
Node `x2` depends on node `x1` because it uses `x1` for computation. Since each block
has `in: List[Sym]` and `res: Exp`, then we know that if `res` depends on node A, A must
be scheduled in or before the block. In addition, if node A depends on the `in: List[Sym]`,
then it must be scheduled in the block (if it is needed by the block). For example:

``` scala
val x1 = 10 + x0
val x2 = x1 + x3
Block([x3], x2, _, _)
```
means that the block needs to return `x2` as the result, thus node `x2` and node `x1`
must be scheduled in or before the block. Also since node `x2` depends on `x3`, which is
an input symbol of the block, it must be scheduled in the block. So we have 2 possible
schedules for this block:

``` scala
val x1 = 10 + x0
block { x3 =>
  val x2 = x1 + x3
  x2
}
```

or

``` scala
block { x3 =>
  val x1 = 10 + x0
  val x2 = x1 + x3
  x2
}
```

and we can choose the more optimized version of them during graph traversal.

What is control dependency? Instead of offering a definition, I will just enumerate some
examples:

1. If we have 2 print statements in the IR, we cannot generate the code with any one of them missing or in the wrong order.
2. If we have a mutable variable or an mutable array, the reading and writing of the mutable elements need to be controled carefully. Some of the reading and writing can be removed or re-ordered, but certainly not all of them.
3. more ???

We talked about "the wrong order" in entry 1. What is the right order? Well, we have
the order of user-code (or the order of LMS IR nodes construction) as the golden standard.
Some of the order can be swapped, but some cannot. Let's just see some examples:

``` scala
var x = 0
x = 3
x = 10
print(x)
```

In this small example, the node `x = 10` is not *data-depended* by the print statement.
However, it is *control-depended* because the value of `x` is mutated by it. However,
the `x = 3` node can be removed without breaking the code semantics. The order of
graph construction (the fact that user did `x = 3` before `x = 10`) is the base of
our control dependency analysis.

Directly keeping track of all control dependencies during LMS IR construction can be
heavy. In our implementation, we keep track of the *effects* of nodes and blocks, and
then compute *dependencies* from *effects* when we need to analyze dependencies. The
design of effects (and effect systems) is an important project currently under development.
We will talk about the current effect system in details later, but for now, I just want
to get one thing across: the `ein: Sym` of `case class Block`.

What is the `ein: Sym` in block? Well, `ein` stands for `effect-input`. Previously, we
talked about normal inputs of blocks `in: List[Sym]`, and we said that "if a node depends
on `in`, it must be scheduled in the block, if ever needed". Similarly, if a node depends
on `ein`, it must be scheduled in the block too, if ever needed. However, `ein` is not
a real input. It is just an effect-input, which is used to control the scheduling of
nodes when a node must be in a block by control-dependencies. For instance:

``` scala
fun { x =>
  print("hello world")
  x
}
```

In this block, the print statement must be in the function block because we expect every
call of the function to print `hello world`. However, the print statement doesn't depend
on the block input `x`. In this case, we let the print node depends on the `ein` of the
block, so that it will be scheduled in the block, but not before.


## Sea Of Nodes

The LMS IR follows the "sea of nodes" design (citations?), where the IR is composed of
a list of `Node`s, and the `Block`s do not explicitly scope the nodes.
That is to say, instead of explicitly scoping the IR constructs that should stay in a `Block`,
the `Block` implicitly express the IR constructs within it via describing the
`inputs` to the `Block` (`in: List[Sym]`), the `results` of the `Block` (`res: Exp`),
the `input-effect` of the `Block` (`ein: Sym`), and the `effects` of the `Block`
(`eff: EffectSummary`).

### Graph Dependencies

But how do the effects help scoping the nodes in a block? The details are in the lms/core/travers.scala
file but we can talk about it in the high level for now.

Generally speaking, the `Node`s in the LMS IR constructs a graph that shows the *data dependencies*
among all `Node`s. However, data dependency is not the only dependency that should be respected.
Another important dependency is *control dependency*. For instance, when there are two print
`Node`s in the LMS IR, both should be generated and in the same order. When there is a mutable variable
in the LMS IR, a write `Node` and then a read `Node`, then the read `Node` clearly depends on the
write `Node`. This is not data dependency because the mutable variable already exists before the
write `Node`.

While data dependencies are captured in the graph structure, the control dependencies have to be
captured in other means. LMS core handles control dependencies by tracking the effects of nodes and
blocks (printing, variable read, variable write, et al), and then computed control dependencies based
on the effects of all nodes/blocks. Effects of nodes and blockes are
expressed via accompanying `EffectSummary`. Both dependencies contribute to code scheduling. Here are
a few simple rules:
1. If node A uses node B, then scheduling A means B must be scheduled before A.
2. If a block's results or effects depend on node A, then A must be schedule within or before this block.
3. If a node depends on the inputs or input-effect of a block, then the node is bound by
this block (i.e., it must be scheduled within this block).

### Example: One Effect System

Here is one example of scala snippet:

``` scala
def snippet(x: Int, y: Int) = {
  var idx = 0
  var agg = 0
  while (idx < x) {
    val p = y * y
    agg = agg + p * idx
    idx += 1
  }
  agg
}
```

If we determine that every statement from this snippet depends on its previous statement
(there is one kind of effect -- the original order of LMS IR creation),
we can have a list of nodes and blocks with the following dependencies.

``` scala
Graph(nodes, Block([x0, x1], x19, x2, [dep: x19]))

nodes = [
  x3 = "var_new" 0    [dep: x2]   // idx
  x4 = "var_new" 0    [dep: x3]   // agg

  x18 = "W" Block([], x7, x5, [dep: x7])
            Block([], (), x8, [dep: x17]) [dep: x4]

    x6 = "var_get" x3       [dep: x5]
    x7 = "<" x6 x0          [dep: x6]

    x9 = "*" x1 x1          [dep: x8]
    x10 = "var_get" x4      [dep: x9]
    x11 = "var_get" x3      [dep: x10]
    x12 = "*" x9 x11        [dep: x11]
    x13 = "+" x10 x12       [dep: x12]
    x14 = "var_set" x4 x13  [dep: x13]
    x15 = "var_get" x3      [dep: x14]
    x16 = "+" x15 1         [dep: x15]
    x17 = "var_set" x3 x16  [dep: x16]

  x19 = "var_get" x4  [dep: x18]
]
```

In this example, we first have two nodes that declare to new variables (x3 and x4).
Then we have a while loop (x18) that has two blocks (the condition block and the
loop body block). The condition block has input-effect x5 and result x7. So it scopes
node x6 and x7 into the condition block. The loop body block has input-effect x8
and effects x17, so it scopes node x9 to x17 in the loop body block.
The while loop node depends on x4, and the final node x19 depends on x18.
Thus a linear ordering of all nodes with two blocks is fully determined by the
dependencies.

### Example: Effects in Categories

To make our effect summary more fine-grained (so that more interesting code reordering and
optimization can be introduced), the first step is to realize that there are different
kinds of effects. The effects of printing is clearly not so related to the effect of variable
reading, thus there should be no scheduling enforcement between them.
We chose to support the following categories of effects:

1. Statements that create mutable objects belong to the category keyed by STORE.
2. Statements that read/write a mutable object belong to the category keyed by the symbol of this object
      (result of an allocation node).
3. For all remaining effectful statements (such as printf), they belong to the category keyed by CTRL
     (for control flow dependent).

#### From Effects to Dependencies

After collection read and write effects, we need to compute dependencies from them. The
rules for computing dependencies from effects are listed below:

1. Read-After-Write (RAW): there should be a hard dependency from R to W,
   since the correctness of the R depends on the W to be scheduled)
2. Write-After-Read (WAR): there should be a soft dependency from W to R,
  since the correctness of the W does not depend on the R to be scheduled, but the order of them matters.
3. Write-After-Write (WAW): the first idea is to generate a soft dependency,
  since the second W simply overwrite the first W.
  However, write to array is more complicated, such as arr(0) = 1; arr(1) = 2,
  where the second W doesn’t overwrite the first W, and both Ws have to be generated.
  For now, we just issue a hard dependency from the second W to the first W.
4. Read-After-Read (RAR): there is no effect dependency between them.

Note that we introduced soft dependencies in the rules.
Soft dependencies are soft in the sense that, if node A soft-depends on node B,
node B cannot be scheduled after A. However, scheduling A does not ensure that B is scheduled.

#### Const Effects

The STORE and CTRL keys are also handled in our read/write system in a case-by-case manner.
For instances, allocating heap memory can be modeled as a read on the STORE key,
if we don’t care about the order of heap allocation.
However, printf should be modeled as write on the CTRL key, since all prints should be
scheduled in the same order.
Some trickier cases include getting a random number from a pseudo-random generator.
It really depends on the expected behavior.

More details about effect system can be found and Gregory's write up:
\url{https://hackmd.io/_-VGqPBiR3qToam7YTDdRw?view}.

#### Latent Effects

Effects can be latent, such as the effects of a function block. The function block may have printing
statements and variable reads/writes, but they are not happening until they are called in function
applications. Tracking latent effects can be tricky if we allow higher-order functions, where functions
can be returned from other functions and control flows, passed in as parameters, and stored in data
structures such as arrays, lists, and et al. The current approach in LMS_clean doesn't support
higher-order functions. LMS application using higher-order functions in LMS IR will trigger errors in
Latent Effect analysis.

#### Effects in Data Structures

LMS IR supports `Array`, which is a mutable data structure holding a fixed number of same typed elements
in consecutive memory. The effects on arrays are at the whole-array granularity (reading and writing to
an element is treated as reading and writing to the whole array). This is less ideal.

LMS IR also supports other data strucutures (including `List`, `Tuple`, `Map`, et al). These data
structures are immutable, pure data structures, thus no effects are needed to use them.

### Watch Out (Silent Failures in Effect System)

The current LMS-clean effect system cannot handle, and silently fails on applications with Aliasing.
Taking the following two code snippet as examples.

``` scala
// aliasing
val arr = new Array[Int](10)
val arr2 = arr.slice(0, 5)
arr2(0) = 5
printf("%d\n", arr(0))
```

``` scala
// borrowing
val arr = new Array[Int](10)
var arr2 = arr
arr2(0) = 5
printf("%d\n", arr(0))
```

In both examples, the write effects on `arr2` should be on `arr` as well. However, without alias
analysis, LMS-clean is not aware that the write effects on `arr2` is on `arr`. As a result, the
write node is discarded in the generated code. Unfortunately, the failure is silent!

The current rational is that it is the users' responsibility to avoid aliasing or borrowing in LMS
applications. Using them results in *Undefined Behaviors*, including generating code that is invalid and
cannot compile. It is interesting to see how Rust like syntax (type-checking) can be intergrated in
LMS-clean to handle alias.

#### Tiny summary of Rust ownership/borrow/slice

Rust heap objects associate with ownership. When the ownership goes out of scope, the heap object is
deallocated. Rust allows references of heap objects to be borrowed. The life time of reference must
be smaller than that of the original object. References are immutable by default, but can also be
mutable. When the reference is mutable, there can only be one reference at a time.

The Rules of References:
1. At any given time, you can have either one mutable reference or any number of immutable references.
2. References must always be valid.

Slices are immutable views of the array.

### Sea of Node

Using "sea of nodes" has perfound implications to LMS IR. The advantage is that
the scopes of nodes are determined dynamically based on correctness and using frequency,
which allows easy code-motion and optimization. However, using "sea of nodes" means that
we must have a way to dynamically determine what nodes are in each block. We compute that
based on *dependencies*.  It does make IR traversal and transformation different from
IRs with explicit scopes, which we will talk about in lms/core/traversal.scala.


### Building EffectSummary in LMS Grap

Our fine-grained current EffectSummary is like below.
It tracks soft dependencies (`sdeps: Set[Sym]`),
hard dependencies (`hdeps: Set[Sym]`),
read keys (`Set[Exp]`), and write keys (`Set[Exp]`).

``` scala
case class EffectSummary(sdeps: Set[Sym], hdeps: Set[Sym], rkeys: Set[Exp], wkeys: Set[Exp])
```

In this section, we will talk about how LMS Graph are constructed using the LMS IR components.
It shows how the LMS IRs are used in constructing LMS Graphs, and how effects and dependencies are
tracked and generated.
All LMS snippets are function. As the result, all LMS Graph have a list of nodes
(already in topological order)
and a block describing the function. That is captured by the

``` scala
case class Graph(val nodes: Seq[Node], val block: Block, val globalDefsCache: immutable.Map[Sym,Node])
```

at lms/core/backend.scala. The LMS Graph is constructed by `class GraphBuilder` at lms/core/backend.scala.

Besides the basic functionality of storing nodes, searching nodes by symbols, and generating fresh symbols,
GraphBuilder offers two keys functionalities

1. Building nodes by the @reflect*@ family of methods.
2. Building blocks by the @reify*@ family of methods.

The core reflect method is defined as below (with some simplification):

``` scala
def reflectEffect(s: String, as: Def*)(readEfKeys: Exp*)(writeEfKeys: Exp*): Exp = {
    // simple pre-construction optimization
    rewrite(s, as.toList) match {
      case Some(e) => e // found optimization (resulting in pure expressions only)
      case None => // no available optimization
        val (latent_ref, latent_wef) = getLatentEffect(s, as:_*)
        val (reads, writes) = ((latent_ref ++ readEfKeys).toSet, (latent_wef ++ writeEfKeys).toSet)

        if (reads.nonEmpty || writes.nonEmpty) {
            // build node with the help of `gatherEffectDeps'
            val (prevHard, prevSoft) = gatherEffectDeps(reads, writes, s, as:_*)
            val summary = EffectSummary(prevSoft, prevHard, reads, writes)
            val res = reflect(Sym(fresh), s, as:_*)(summary)

            // update effect environments (curEffects, curLocalReads, and curLocalWrites)
            curLocalReads ++= reads
            curLocalWrites ++= writes
            for (key <- reads) {
              val (lw, lrs) = curEffects.getOrElse(key, (curBlock, Nil))
              curEffects += key -> (lw, res::lrs)
              if (key == STORE) curEffects += res -> (res, Nil)
            }
            for (key <- writes) { curEffects += key -> (res, Nil) }
            res
        } else {
            // We can run Common Subexpression Elimination (CSE) for pure nodes
            findDefinition(s,as) match {
                case Some(n) => n.n
                case None =>
                reflect(Sym(fresh), s, as:_*)()
            }
        }
    }
}
```

This method first tries to run pre-construction optimization via `rewrite`.
The optimized result is a pure Exp that can be returned directly. If not optimized,
the method computes the latent effects via `getLatentEffect` helper method,
and then combine the results with the user provided `readEfkeys` and `writeEfKeys`.
If the effect keys are empty, the methods go to the else branch and try Common
Subexpression Elimination (CSE) via `findDefinition` helper method. Otherwise,
the method compute dependencies from the effect keys via `gatherEffectDeps` method
and then creates the node. The last thing to do is updating the effect environments,
including `curEffects`, `curLocalReads`, and `curLocalWrites`. The `curEffects` is
a map of type: `Exp -> (Sym, List[Sym])`, which tracks the *last write* and the *reads
after last write* for each Exp key.

The `getLatentEffect` family of methods are like below. They essentially recursively
call each other to dig out all latent effects of nodes.

``` scala
def getLatentEffect(op: String, xs: Def*): (Set[Exp], Set[Exp]) = (op, xs) match {
    case ("lambda", _) => (Set[Exp](), Set[Exp]()) // no latent effect for function declaration
    case ("@", (f: Sym)+:args) => getApplyLatentEffect(f, args:_*)._1
    case _ => getLatentEffect(xs:_*)
}
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
def getFunctionLatentEffect(f: Exp): ((Set[Exp], Set[Exp]),(Set[Int], Set[Int]), Option[Exp]) = findDefinition(f) match {
      case Some(Node(_, "lambda", List(b:Block), _)) =>
        getEffKeysWithParam(b)
      case Some(Node(_, "lambdaforward", _, _)) => // what about doubly recursive?
        ((Set[Exp](), Set[Exp](Const("CTRL"))), (Set[Int](), Set[Int]()), None)
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
      case Some(e) => ???
}
def getLatentEffect(xs: Def*): (Set[Exp], Set[Exp]) =
    xs.foldLeft((Set[Exp](), Set[Exp]())) { case ((r, w), x) =>
      val (ref, wef) = getLatentEffect(x)
      (r ++ ref, w ++ wef)
    }
def getLatentEffect(x: Def): (Set[Exp], Set[Exp]) = x match {
    case b: Block => getEffKeys(b)
    case s: Sym => findDefinition(s) match {
        case Some(Node(_, "lambda", (b@Block(ins, out, ein, eout))::_, _)) => getEffKeys(b)
        case _ => (Set[Exp](), Set[Exp]())
    }
    case _ => (Set[Exp](), Set[Exp]())
}
```

However, the complex code here is not yet fully correct. There are several issues:

1. We have a specical function (`getApplyLatentEffect`) to dig out latent effects
      of functions (since they are applied via `@` syntax). However, the LMS IR may
      have other syntax to apply a function, such as `("map", List(array, f))`.
      It is still not clear when to dig latent effects out and when to not.
      Conservatively we have to always do it.
2. Depending on whether we want to support first-order functions, the functions
      may be wrapped in complex data structures (as array element) or returned from
      other functions or conditionals. The `getFunctionLatentEffect` function might have
      too many cases to check and too many branches to consider, which is both expensive
      and inaccurate.
3. The read and write effects on data structures are currently at the most coarse
      granularity (for the whole data structure). Also the aliasing and borrowing effects
      are not yet considered.

Potential solutions are listed here:
1. be conservative and cheap at some cases with a *stop the world* effect
2. track aliasing with some frontend constraint (like rust, separation logic, regions)

The `gatherEffectDeps` method computes dependencies from effect keys:

``` scala
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
```

Note that the `reify*` family of methods not only generate the Block object, but also the nodes that are
used in the block. However, the nodes are not explicitly scoped in the block, but rather implicitly
scoped via effect summaries. This implicit scoping allows flexible code motion as long as effects and dependencies
are respected.
The `withBlockScopedEnv` method is the subroutine that saves old effect environments.

``` scala
def reify(arity: Int, f: List[Exp] => Exp, here: Boolean = false): Block =
withBlockScopedEnv(here){
    val args = (0 until arity).toList.map(_ => Sym(fresh))
    val res = f(args)
    // remove local definitions from visible effect keys
    val reads = curLocalReads.filterNot(curLocalDefs)
    val writes = curLocalWrites.filterNot(curLocalDefs)
    var hard = writes.map(curEffects.map(_)._1)
    if (curEffects.map contains res) // if res is a local mutable (e.g. Array)
      hard += curEffects.map(res)._1
    if (hard.isEmpty)
      hard = Set(curBlock)

    Block(args, res, curBlock, EffectSummary(Set(), hard, reads, writes))
}
```

