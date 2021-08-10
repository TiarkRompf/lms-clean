The `lms/core/traversal.scala` file introduces how to traverse the LMS IR. Travering a tree-structured IR seems trivial, but traversing LMS IR (sea-of-nodes) is not. The main challenge is to determine the group of nodes that should be scoped in each block. The `Traverser` class introduces a basic version of scheduling using frequency estimation. `CompactTraverser` further inlines nodes with regard to their dependencies.

Besides traversing the IR, we can also lower the IR into a lower-level representation. This is handled by the `Transformer` class, which is built on top of a traverser.

# Traverser
Previously in [Backend](backend.md), we
talked about how the scopes of the blocks are implicitly expressed via data dependency and control
dependencies. In `traversal.scala`, we will see how exactly we schedule the nodes for each block using
the dependency information.

The basic idea is as follows:
1. Each block has input-variables and an input-effect-variable. We call them bound variables of a block (see backend.scala).
2. A node is ready to be scheduled for the current block when the bound variables on which the node depends are already available in the current block.
3. A node should be scheduled in the current block instead of in a inner block when the node is used often.

### A recap of dependencies
In LMS IR, we have two categories of dependencies: data dependency and control dependencies. Data dependency captures node usage (e.g. node A uses symbol B). This information is already available in the definition of a node (i.e. `rhs: List[Def]`). Control dependencies are dependencies caused by effects. For example, two print statements should be generated in the original user-defined order, or the read/write order of a mutable cell cannot be randomly shuffled. We track control dependencies via `EffectSummary` (see details in [Backend](backend.md)).
Control dependencies can be further categorized into *soft* and *hard* dependencies.
If node A hard-depends on node B, then B must be scheduled before A.
If node A soft-depends on node B, then B should never be scheduled after A, but B might not be scheduled even
if A is scheduled.

### Dead Code Elimination (DCE)
In a real scenario, we first run a DCE pass before we traverse the graph.
The DCE pass is currently implemented in `class DeadCodeElimCG` (in file `lms/core/backend.scala`).

The basic idea for DCE is like this. We track two categories of `Sym`s, the *live* and *reach* sets.
1. The reach set is the set of nodes (or Syms of the nodes) that are reachable via hard dependencies.
It includes both data dependencies and control dependencies.
2. The live set is the set of nodes (or `Backend.Sym`s of the nodes) that are needed only via data dependencies.

Then we run three steps for DCE:
1. Collect reach set and live set. The two sets are initialized via block result and block effects. Then
   on a reverse iteration of all nodes, we collect nodes that are reachable (in reach set) and fill the
   live Syms and reach Syms from the nodes. We also collect all reachable nodes in `newNodes`.
2. Collect used Exp set, which means all Exp that are used (including `Const`).
3. Rebuild graph with `newNodes` and old block, but with all effects/dependencies filtered to remove unused Exps.

A more detailed code walk can be found in backend.md

### Bound Variables
We first calculate the set of *bound variables* for each node.
We decide when a node is ready to be scheduled for a block by checking its bound variables. We implement bound variable calculation as an analysis pass, as shown in `class Bound extends Phase` in lms/core/backend.scala. We use a hashmap `hm` to keep track of the bound variables of all symbols in a graph.

As shown in the `apply` method, bound variable calculation is done by the following steps:
1. Collect all `Block`s in the graph `g`, including those defined in the nodes of `g` and the input block of `g`. Take the union of the bound variables (input and effect input) of these blocks. We define the bound variables of these new symbols to be just themselves, since they are just inputs.
2. For each node in `g`, get all bound variables it depends on and all bound variables generated in this node (if it has blocks). The difference of these two sets is the bound variables it really depends on.
3. We need to get "lambda-forward" node (the forward declaration of "λ") to have the same bound
   variables as "λ". This needs to be fixed in a Convergence Loop (unfortunately).

### Node Frequency Estimation
Consider we have a node of heavy computation in a block. Suppose we have determined that the node has no dependency on the inputs of the block. Should we schedule it inside or outside the block? The answer is that we should consider how often the node will be executed. If the block is a function body, then it can be called multiple times, so it would be beneficial to schedule it in the outer scope. If the block is a conditional block, then scheduling the node outside means that the node would always be executed regardless of the condition. So we should schedule it inside the block in this case. Therefore, we can assign a number representing relatively how often a node is executed, and only schedule the nodes that have a large enough frequency. This is the intuition of frequency estimation.

The `symsFreq` function in `Traverser` achieves this: Given a lambda node containing a block, we assign the hard dependencies of the effect summary of the block to a large enough number 100, representing that the body of a function is executed often. For a conditional node, we assign the results of both if and else block to 0.5, assuming each branch is taken with equal probability. For a loop block, we assign the results of the conditional and body block to 100, assuming the loop is iterated many times. For a switch node, we treat it similar to the conditional node by assigning each branch to 0.5. Lastly, all other nodes are assigned 1.0.


### Traversal Abstract Class

The core functionality for scheduling the nodes for a block is the function named
`def scheduleBlock_[T](y: Block, extra: Sym*)(f: List[Sym], Seq[Node], Seq[Node], Block) => T): T`

The function works with two mutable states: the `path` and `inner`. The `path` refers to the
accumulated bound variables when we are in nested scopes, and the `inner` refers to the list of
nodes to be selected for scheduling. The `withScope` function helps maintain the
two mutable states when entering or exiting from a scope.

In the `scheduleBlock_` function, it first updates the `path` by

``` scala
val path1 = y.bound ++ extra.toList ++ path
```
where `y.bound` returns the bound variable of the current block, `extra.toList` returns the extra symbols
that we see as available bound variables, and `path` is the old available bound variables before entering
this current block.

Then we set up a filter for nodes considered *available*:

``` scala
def available(d: Node) = bound.hm(d.n) -- path1 - d.n == Set()
```
which is to say, a node is available when the bound variables of the node are all in the `path`.

#### Calculate Reachability Information
Then we compute two sets of symbols, the `reach` (for nodes that are reachable from the current block)
and the `reachInner` (for nodes that are reachable from an inner block).
The `reach` set is initialized as `y.used`, which is the hard-dependencies and result of the block, if
the result is a symbol rather than a constant.
The `reachInner` is not initialized immediately, but is seeded when a reachable and available symbol has
low frequency (such as should rather be scheduled in an if-true block or if-false block).

The accumulation of `reach` and `reachInner` is simple: reverse traveral reachability

``` scala
for (d <- g.nodes.reverseIterator) {
   if (reach contains d.n) {
      if (available(d)) {
         // a check such that node with low frequencies are in inner blocks
         for ((e:Sym,f) <- symsFreq(d))
            if (f > 0.5) reach += e else reachInner += e
      } else {
         // if not available, never put in `reachInner`
         // d's hard dependencies may be available
         reach ++= hardSyms(d)
      }
   }
   if (reachInner.contains(d.n)) {
      reachInner ++= hardSyms(d)
   }
}
```

#### Scheduling
Then we try to compute the `val outer1 = Seq[Node]()` (nodes scheduled for the currents
and the `val inner1 = Seq[Node]()` (nodes scheduled for inner blocks).
The basic logic is: nodes that are reachable and available should be in `outer1`.
Nodes that are reachable but not available, or reachable by `reachInner` should be in
`inner1`. We need to take care of soft dependencies too. Previously we said that
soft dependencies do not enforce scheduling, just the order. So it might be confusing
as to why we are considering soft dependencies too. It turns out that the dead code elimination
pass should have removed soft dependencies that are not necessary to be scheduled. The
remaining soft dependencies are nodes that should actually be scheduled.

The code that does so is here:

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

#### Example

Let us go over an example of scheduling a simple program. The source program is the following:
```scala
def snippet(arg: Rep[Int]) = {
  printf("hello, world1")
  printf("hello, world2")
  if (arg > 0) {
    printf("hello, world3")
    arg + 1
  } else {
    arg + 2
  }
}
```
Starting from the top-level block, we have the following nodes:
```scala
================ Block(List(x1),x10,x0,[CTRL*: _ | x10]) ==============
x2 = (printf hello, world1)  [CTRL*: _ | x0]
x3 = (printf hello, world2)  [CTRL*: _ | x2]
x4 = (> x1 0)  
x6 = (printf hello, world3)  [CTRL*: _ | x5]
x7 = (+ x1 1)  
x9 = (+ x1 2)  
x10 = (? x4 Block(List(),x7,x5,[CTRL*: _ | x6]) Block(List(),x9,x8,[: _ | x8])) [CTRL*: _ | x3]
```
Note that the dependency information is already calculated by the Backend (see the graph construction section of Backend.md).

In the graph, the top-level block takes `x0` as effect input, takes `x1` as data input (arg), and returns `x10` as the result. The block has a write effect to `CTRL`, and a hard dependency on `x10`.
In the block, we have 7 nodes listed in the same order as in the source program.

We first calculate `path1`. Since we are at the top block, `path` is an empty set, so `path1` just contains the input and effect input of the top-level block, i.e., `x0, x1`.

We then determine `reach` and `reachInner` by traversing the nodes backward. We initialize `reach` to contain the result node `x10` and reachInnder as an empty set. Since `x10` is reachable and available, we call `symsFreq` on `x10`. Since `x10` is a conditional node, we assign the condition(`x4`) and the hdep(`x3`) to 1.0, and assign the results(`x7`, `x9`) and hdeps(`x6`, `x8`) of two branch blocks to 0.5. At this point:
```scala
reach = [x10, x4, x3]
reachInner = [x7, x9, x6, x8]
```
We then consider the next node `x9`. Since it's already in `reachInner`, we add its hardSyms `x1` into `reachInner`. Now we have:
```scala
reach = [x10, x4, x3]
reachInner = [x7, x9, x6, x8, x1]
```
Then we consider `x7`. It also adds `x1` into `reachInner`, which doesn't change `reachInner`. Then we consider `x6`. Since `x6` is in `reachInner`, we add its hdep `x5` into `reachInner`. Now we have:
```scala
reach = [x10, x4, x3]
reachInner = [x7, x9, x6, x8, x1, x5]
```
We then consider `x4`. Since it's in reach and it's available, we add its ddep `x1` in `reach`. So:
```scala
reach = [x10, x4, x3, x1]
reachInner = [x7, x9, x6, x8, x1, x5]
```
Then we consider `x3`, which adds `x2` into `reach`:
```scala
reach = [x10, x4, x3, x1, x2]
reachInner = [x7, x9, x6, x8, x1, x5]
```
Lastly, we consider `x2`, which adds `x0` into `reach`. Finally, we have:
```scala
reach = [x10, x4, x3, x1, x2, x0]
reachInner = [x7, x9, x6, x8, x1, x5]
```

After having gathered the reachability information, we traverse the nodes again in backward and put them into either `outer1` or `inner1`. Following the algorithm, we have `[x10, x2, x3, x4]` scheduled in `outer` and `[x6, x7, x9]` scheduled in inner.

We now consider the if-block in `x10`. First, we add `x5` into path, and path now becomes `x5, x1, x10`. Then, we re-calculate `reach` and `reachInner`, which become `[x7, x1, x6, x5]` and `[]`, respectively. We schedule `x6` and `x7` to `outer1`, while `x9` is considered unreachable. `inner1` remains empty, so we exit this block and enter the else branch. Similarly, we recalculate `reach` as `x1, x9, x8` and schedule `x9` into this block.

# Compact Traverser

The `Traversal` abstract class provided the basic functionalities for scheduling nodes
of a given block. It can be used in graph transformation and code generation.
We will discuss transformation later, but for code generation, the caveat is that
the basic `Traveral` class will generate each node as a statement, leading to code
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

// for nodes that are known to be scheduled for inner blocks,
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
     // mayInline is the user-level inline control (i.e. user has the option to override inlining)
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


# Transformer

A transformer is a traverser that builds another LMS IR while traversing. Transformers are needed when we want to lower the input program to a low-level language. For instance, a high-level machine learning program written in a high-level IR can be lowered multiple times until we are ready to emit C/C++ code that can be readily compiled and executed.

In this section, we introduce the basic `Transformer` class, which is an identical transformation. This identical transformer serves as a basis and more interesting transforms (e.g. optimizations) can be implemented on top of it.(heads-up, it is more complex than you probably think).

The transformer works on two key objects: 1) a new `GraphBuilder` and 2) a hashmap `subst` mapping old symbols to new expressions. A `GraphBuilder` provides multiple ways of adding nodes and blocks into a Graph IR (see backend.md).

The core of `Transformer` is a family of `transform` functions:
```scala
def transform(s: Exp): Exp
def transform(b: Block): Block
def transformRHS(rhs: List[Def])
def transform(n: Node): Exp
def transform(graph: Graph): Graph
```
They maps structures (`Exp`, `Block`, etc) in the old IR into their counterparts in the new IR.

### Transforming *Exp*

The following code snippet shows how to transform an `Exp`. For a `Sym`, we go to the `subst` map and find its counterpart in the transformed IR. We print a warning message if the `Sym` is not found in `subst`. For a `Const`, we just return itself since `Const`s are immediate values.

``` scala
  def transform(s: Exp): Exp = s match {
    case s @ Sym(_) if subst contains s => subst(s)
    case s @ Sym(_) => println(s"Warning: not found in subst $subst: "+s); s
    case a => a // must be const
  }
```

### Transforming Blocks
When transforming a block, we need to reify the block by traversing it.

We use the `reify` method of `GraphBuilder` to transform a block. The signature of `reify` is:
```scala
def reify(arity: Int, f: List[Exp] => Exp, here: Boolean = false): Block
```
the method takes a function `f` as argument and applies `f` with `args` number of fresh `Syms` as arguments to `f`. The returned value is the new result of the block. The method then figures out other information of the new block (see backend.md).

So what should `f` be for `transform`? If a block does not have inputs, we just traverse the block and transform the `res`. Otherwise, if the block has an input, we update `subst` to map it to the new input, which is the formal parameter `e`. In `reify`, `e` will be instantiated with a fresh `Sym`. We then traverse the block and transform the `res` with the updated `subst` and remove the block input after the transformation is done.

``` scala
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
```

### Transforming Nodes

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

For other kinds of node, we first transform the RHS of the node by calling the appropriate `transform` function for each case. Then, we transform the effect keys and call `reflect`.

### Traverse Nodes

We override the `def traverse(n: Node): Unit` function to update the `subst` map such that the transformed node is tracked. We also update the typeMap and sourceMap accordingly.

``` scala
override def traverse(n: Node): Unit = {
  subst(n.n) = transform(n)
  // Also pass the metadata to the new exp
  // We use `getOrElseUpdate` because the handling of each node might
  // have already filled a proper value, which we don't want to override
  if (Adapter.oldTypeMap.contains(n.n))
    Adapter.typeMap.getOrElseUpdate(subst(n.n), Adapter.oldTypeMap(n.n))
  if (Adapter.oldSourceMap.contains(n.n))
    Adapter.sourceMap.getOrElseUpdate(subst(n.n), Adapter.oldSourceMap(n.n))
}
```

### Transforming Graph

We finally present how to transform a graph. Firstly, we need to cache `globalDefsCache`, `typeMap` and `sourceMap`. Then, we reify the top-level block similar to what we do in the previous section (Transforming Blocks). In addition, we assert that the top-level block has only one input. We then call `wrapGraph(graph)`, which calls the `apply` method of the superclass to traverse the graph. Lastly, we transform the `res` of the top-level block. Finally, we need to merge the old and new metadata and return the transformed graph.

```scala
def transform(graph: Graph): Graph = {
  // XXX unfortunate code duplication, either
  // with traverser or with transform(Block)

  // Handling MetaData 1. save oldTypeMap/SourceMap first
  Adapter.oldDefsCache = graph.globalDefsCache
  Adapter.oldTypeMap = Adapter.typeMap
  Adapter.oldSourceMap = Adapter.sourceMap

  // Handling MetaData 2. initialize MetaData as fresh, so the transformer might add new metadata entries
  Adapter.typeMap = new mutable.HashMap[Backend.Exp, Manifest[_]]()
  Adapter.sourceMap = new mutable.HashMap[Backend.Exp, SourceContext]()

  val block = g.reify { e =>
    assert(graph.block.in.length == 1)
    subst(graph.block.in(0)) = e
    // subst(graph.block.ein) = g.curBlock.head // XXX
    wrapGraph(graph)
    transform(graph.block.res)
  }

  // Handling MetaData 3. update new metadata with old metadata
  // this is not redundant because of the block arguments (function definitions)
  for ((k, v) <- subst if v.isInstanceOf[Sym] && Adapter.oldTypeMap.contains(k))
    Adapter.typeMap.getOrElseUpdate(v, Adapter.oldTypeMap(k))
  for ((k, v) <- subst if v.isInstanceOf[Sym] && Adapter.oldSourceMap.contains(k))
    Adapter.sourceMap.getOrElseUpdate(v, Adapter.oldSourceMap(k))

  Graph(g.globalDefs,block, g.globalDefsCache.toMap)
}
```

