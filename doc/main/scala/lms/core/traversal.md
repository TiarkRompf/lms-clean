## Traverse

The `lms/core/traversal.scala` define how one can traverse the LMS IR. Travering a tree-structured
IR seems trivial, but traversing LMS IR (sea of the nodes) is not. The main challenge is to determine
the group of nodes that should be scoped in each block. Previously in [Backend](backend.md), we
talked about how the scope of the blocks are implicitly expressed via data dependencies and control
dependencies. In `traversal.scala`, we will see how exactly we collect the nodes for each block using
the dependency information.

The basic idea is as follows:
1. Each block has input-variables and input-effect-variables. We call them *bound variables*.
2. A node is ready to be schedule for the current block when the bound variables the node depends on are already in path (those blocks are enclosing the current block).
3. A node should be scheduled in the current block (instead of in a inner block) when the node is used often.

### A recap of dependencies
In LMS IR, the data dependencies refer to dependencies of data (node A uses symbol B).
There are also control dependencies, such as two print statements should be generated in the same order,
or the read/write order of a mutable cell cannot be randomly shuffled.

Data dependencies are explicitly presented in the LMS IR. However, control dependencies have to be tracked
via Effect Systems (see details in [Backend](backend.md)).
At LMS IR construction, we track the various categories of effects of the LMS nodes, and then build dependencies
from the effects. The dependencies are captured via either *soft-dependency* or *hard-dependency*.
If node A hard-depends on node B, then scheduling A means B must be scheduled before A.
If node A soft-depends on node B, then B should never be scheduled after A (but B might not be scheduled even
if A is scheduled).

### Dead Code Elimination

In a real scenario, we first run a dead code elimination (DCE) pass before we traverse the graph.
The DCE pass is currently implemented in `class DeadCodeElimCG` (in file `lms/core/backend.scala`).

The basic idea for DCE is like this. We track 2 categories of `Backend.Sym`s, the *live* and *reach* sets.
1. The reach set is the set of nodes (or Syms of the nodes) that are reachable via hard-dependencies.
It includes both data dependencies and control dependencies.
2. The live set is the set of nodes (or `Backend.Sym`s of the nodes) that are needed only via data dependencies.

Then we run three steps for DCE:
1. Collect reach set and live set. The two sets are initialized via block result and block effects. Then
   on a reverse iteration of all nodes, we collect nodes that are reachable (in reach set) and fill the
   live Syms and reach Syms from the nodes. We also collect all reachable nodes in `newNodes`.
2. Collect used Exp set, which means all Exp that are used (including `Const`).
3. Rebuild graph with `newNodes` and old block, but with all effects/dependencies filtered to remove
   unused Exps.

### Bound Variables

We decide when a node is ready to be scheduled for a block by checking the bound variables. In lms/core/backend.scala `class Bound extends Phase`, we compute the bound variables of each node. This is like an
analysis pass of the LMS IR.

It is done in the following steps:
1. Get the set of all bound variables via `g.nodes.flatMap(boundSyms).toSet ++ g.block.bound` which
   has the bound variables from all nodes and the block.
2. For each node, get all bound variables that it depends on and all bound variables generated in this node (if it has blocks). The diff is the bound variables it really depends on.
3. We need to get "lambda-forward" node (the forward declaration of "lambda") to have the same bound
   variables as "lambda". This needs to be fixed in a Convergence Loop (unfortunately).

### Node Frequency Computation

When a node is ready to be schedule, we can schedule it for the current block if the node is used often
enough. The using frequency is determined by the `class Flow extends Phase` in lms/core/backend.scala.
Note: for now the frequency computation is from the `def symsFreq` in `abstract class Traverser`.

The basic idea is that, if a node is used in only one branch of a conditional, we don't schedule it
in the current block.

### Traversal Abstract Class

The core functionality for scheduling the nodes for a block is the function named
`def scheduleBlock_[T](y: Block, extra: Sym*)(f: List[Sym], Seq[Node], Seq[Node], Block) => T): T`

The function works with two mutable states: the `path` and `inner`. The `path` refers to the
accumulated bound variables when we are in nested scopes, and the `inner` refers to the list of
nodes that are to be selected for scheduling. The `withScope` function helps maintain the
two mutable states when going into or exiting from a scope.

In the `scheduleBlock_` function, it first updates the `path` by

``` scala
val path1 = y.bound ++ extra.toList ++ path
```
where `y.bound` returns the bound variable of the current block, `extra.toList` returns the extra symbols
that we see as available bound variables, and `path` is the old available bound variables before entering
this current block.

Then we set up a filter for nodes that are considered *available*:

``` scala
def available(d: Node) = bound.hm(d.n) -- path1 - d.n == Set()
```
which is to say, a node is available when the bound variables of the node are all in the `path`.

Then we compute two sets of symbols, the `reach` (for nodes that are reachable from the current block)
and the `reachInner` (for nodes that are reachable from an inner block).
The `reach` set is initialized as `y.used`, which is the hard-dependencies and result of the block, if
the result is a symbol rather than a constant.
The `reachInner` is not initialized immediated, but is seeded when a reachable and available symbol has
low frequency (such as should rather be scheduled in an if-true block or if-false block).

The accumulation of `reach` and `reachInner` is simple: reverse traveral reachabiity

``` scala
for (d <- g.nodes.reverseIterator) {
   if (reach contains d.n) {
      if (available(d)) {
         // a check such that node with low frequencies are in inner blocks
         for ((e:Sym,f) <- symsFreq(d))
            if (f > 0.5) reach += e else reachInner += e
      } else {
         // if not available, never put in `reachInner`
         reach ++= hardSyms(d)
      }
   }
   if (reachInner.contains(d.n)) {
      reachInner ++= hardSyms(d)
   }
}
```

Then we try to compute the `val outer1 = Seq[Node]()` (nodes scheduled for the currenta
and the `val inner1 = Seq[Node]()` (nodes scheduled for inner blocks).
The basic logic is that, nodes that are reachable and available should be in `outer1`.
Nodes that are reachable but not available, or reachable by `reachInner` should be in
`inner1`. We need to take care of soft dependencies too. Previously we said that
soft dependencies do not enforce scheduling, just the order. So it might be confusing
as to why we are considering soft dependencies too. It turns out that the dead code elemination
pass should have removed soft dependencies that are not necessary to be schedule. The
remaining soft dependencies are nodes that should actually be scheduled.

The code that does so is here

``` scala
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
```

After computing the set of nodes of the current block, we traverse the nodes one
by one for the block.

### Compact Traversal class

The `Traversal` abstract class provided the basic functionalities for scheduling nodes
of a given block. It can be used in graph transformation and code generation.
We will discuss transformation later, but for code generation, the caveat is that
the basic `Traveral` class will generate each node as a statment, leading to code
that is not very compact, such as

``` scala
int x1 = x0 + 1;
int x2 = x1 * 2;
```

while a human might tend to inline the code to

``` scala
int x2 = (x0 + 1) * 2;
```
for conciseness. This caveat is addressed by `class CompactTraverser extends Traverser`.

The key functionality of this class is the method `var shouldInline: Sym => Option[Node]`.
It returns `Some[Node]` if a symbol should be inlined, and `None` otherwise. It can also
be accessed via this pattern matching object

``` scala
object InlineSym {
   def unapply(x: Sym) = shouldInline(x)
}
```

To handle proper code inlining, we override the `def traverse(ns: Seq[Node], y: Block): Unit`
method. It means that the procedure to schedule the nodes of a block is unchanged from the
`abstract class Traverser`, but when we modify the procedure to generate the list of nodes of
the block to inline some of them. So instead of generating each node one by one, we do the following.

We compute the following analysis results:

``` scala
// lookup Sym -> Node for locally defined nodes
val df = new mutable.HashMap[Sym, Node]

// the number of local uses of a sym
val hm = new mutable.HashMap[Sym, Int]

// local successer nodes (include blocks and effects)
val succ = new mutable.HashMap[Sym, List[Sym]]

// set of symbols used from some inner scope
val hmi = new mutable.HashSet[Sym]
```

They are computed from this snippet

``` scala
// first, count the result of the block as 1 use
if (y.res.isInstanceOf[Sym]) hm(y.res.asInstanceOf[Sym]) = 1
for (n <- ns) {
   // travere all nodes and store them as local nodes
   df(n.n) = n
   // accumulate direct uses (do not count uses via blocks or effects)
   for (s <- directSyms(n) if df.contains(s) || n.op == "λforward")
      hm(s) = hm.getOrElse(s,0) + 1
   // accumulate successer uses (count uses via blocks and effects)
   for (s <- syms(n) if df.contains(s))
      succ(s) = n.n::succ.getOrElse(s,Nil)
   // store block uses as inner uses
   blocks(n).foreach(hmi ++= _.used)
}

// for nodes that are known to be schedule for inner blocks,
// collect their hardSyms as inner uses.
for (n <- inner) hmi ++= hardSyms(n)
```

Then `shouldInline` is defined just like this

``` scala
shouldInline = { (n: Sym) =>
   if ((df contains n) &&              // locally defined
       (hm.getOrElse(n, 0) == 1) &&    // locally used exactly once
       (!hmi(n)))                      // not used in nested scopes
       Some(df(n))
   else None
}
```

But this is not yet complete. We cannot inline if there are conflicts
with dependencies. Consider this example

``` scala
var x = 0
while (x < arg) {
   val y: Rep[Int] = x
   x += 1
   printf("%d\n", y)
}
```
In this example, the `y` is defined locally, only used once by the print statement,
and not used by inner scopes. However, inlining `y` as `x` in the print statement
would be wrong, since the print statement would print the updated value after `x += 1`.
This is a perfect example of inlining conflicting with dependencies.

There is thus another reverse pass that mutates `df` and remove nodes from the
set of locally defined nodes if they conflict with dependencies. The basic idea is
we try to follow the current inlining schedule, and walk in reverse order to see
if any nodes are scheduled after some of the dependents. If so, we disable the inlining
of them by removing them from the `df` hashmap.

We use
``` scala
val seen = new mutable.HashSet[Sym]
```
to track (in reverse order) the nodes that should have been scheduled. This function

``` scala
def checkInline(res: Sym) = shouldInline(res) match {
   case Some(n) =>
     // want to inline, now check that all successors are already there, else disable
     if (mayInline(n) && succ.getOrElse(n.n,Nil).forall(seen))
       processNodeHere(n)
     else
       df -= n.n
   case _ =>
}
```

checks a symbol. If the symbol is to be inlined, we got to make sure that all
the dependents (as collected in the `succ` hashmap) are already seen in the
reverse order (they are schedule after). If not, we have to disable the inlining
of the symbol. If it is inlined, we need to furthur track that node and see if
it inlines more nodes. That is done by this function:

``` scala
def processNodeHere(n: Node): Unit = {
   seen += n.n
   for (s <- directSyms(n).reverse) {
     checkInline(s)
   }
}
```

In using these two functions, we start from the result of the block if it is a symbol
``` scala
if (y.res.isInstanceOf[Sym]) checkInline(y.res.asInstanceOf[Sym])
```
Then we do so for all other non-inlining nodes in the reverse order
``` scala
 for (n <- revNs) {
   if (shouldInline(n.n).isEmpty) {
     processNodeHere(n); numStms += 1
   }
 }
```

Finally we call a `traverseCompact(ns, y)` function to traverse all nodes in this block.
Note that the `traverseCompact` function will only traverse nodes that are not inlined.
Inlined nodes are traversed at the point of inlining.

``` scala
def traverseCompact(ns: Seq[Node], y: Block): Unit = {
    // only emit statements if not inlined
    for (n <- ns) {
      if (shouldInline(n.n).isEmpty)
        traverse(n)
    }
}
```

Note that traversing inlined nodes using the functions with `shallow` in the names,
such as `def shallow` and `def traverseShallow`.


### Transformer

`abstract class Transformer extends Traverser` says that `Transformer` is one kind of
`Traverser`, the kind that builds another LMS IR when traversing. The basic `Transformer`
class is an identical transformation, and we can define transformers on top of it
that add in optimizations or other stuff. In this section, we will see how identical
transformation is done in LMS (heads-up, it is more complex than you probably think).

The transformer will have a new `var g: GraphBuilder` and a map from old symbols to new symbols
`val subst = new mutable.HashMap[Sym, Exp]`.

When transforming an `Exp`, either check the `subst` or return the `Const`.

``` scala
  def transform(s: Exp): Exp = s match {
    case s @ Sym(_) if subst contains s => subst(s)
    case s @ Sym(_) => println(s"Warning: not found in subst $subst: "+s); s
    case a => a // must be const
  }
```

When transforming a block, we need to reify the block by traversing it.

``` scala
  def transform(b: Block): Block = b match {
    case b @ Block(Nil, res, block, eff) =>
      g.reify {
        //subst(block) = g.effectToExp(g.curBlock) //XXX
        traverse(b); transform(res)
      }
    // other cases omitted
  }
```

When transforming a node, we need to transform the right-hand-side and the effects.

``` scala
  def transform(n: Node): Exp = n match {
    case Node(s, "λ", (b @ Block(in, y, ein, eff))::_, _) =>
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
```
The handling of function is incomplete yet, because it is not taking care of recursive functions.

When traversing a node, we need to transform the node, and store the returned symbol to the `subst` map:

``` scala
  override def traverse(n: Node): Unit = {
    subst(n.n) = transform(n)
  }
```

### CPS Transformation

To be continued.
