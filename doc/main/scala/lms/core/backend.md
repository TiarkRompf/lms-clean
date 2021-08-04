The `lms/core/backend.scala` file contains several important aspects of the LMS IR. In `object Backend`, we define the basic building blocks of the IR and effect-related data structures. In `class GraphBuilder`, `class GraphBuilderOpt` and `case class Graph`, we introduce how new nodes are added into a graph IR and how effects are handled and used to calculate dependencies. We also define `Phase` as a framework for graph-to-graph transformation and introduce a dead code elimination (DCE) phase `DeadCodeElimCG`.

# Backend
The core LMS IR is defined in `object Backend`. We first introduce the basic building blocks of the IR:

``` scala
abstract class Def // Definition: used in right-hand-side of all nodes
abstract class Exp extends Def
case class Sym(n: Int) extends Exp   // Symbol
case class Const(x: Any) extends Exp // Constant
case class Block(in: List[Sym], res: Exp, ein: Sym, eff: EffectSummary) extends Def

case class Node(n: Sym, op: String, rhs: List[Def], eff: EffectSummary)
```

`Def` represents a definition, which is used in the RHS of all nodes. A definition can be an `Exp` or a `Block`. `Exp` can be a `Sym` or a `Const`. `Sym` represents symbols in the IR identified by a unique integer. For example, `Sym(2)` represents `x2` in the generated code. `Const` represents immediate values, such as integers (e.g. `Const(0)`), strings (e.g. `Const(hello)`), etc.

`Node` represents statements in the IR. The first field `n` of a node is a `Sym` corresponding to the LHS of a statement. The second field `op` is the operator of the statement. We use strings as operators since they permit easy extension of various kinds of nodes such as `-`, `print`, etc. The third field is the list of operands (RHS) of a node. Let's ignore the last field `EffectSummary` for now, which will be covered later. For example, the statement `val x3 = x2 + 1` would become
``` scala
Node(Sym(x3), "+", List(Sym(x2), Const(1)), _)
```
in the IR.

We can construct programs of arbitrary sizes using `Exp` and `Node`, but there are still no scopes in the IR. We use the `Block` class to represent a scope enclosed by `{...}`. It might be surprising that `Block` does not really contain a list of nodes, which is a common feature in tree-based IRs. For example, the `BasicBlock` in LLVM contains a list of instructions in the block. Instead, our `Block` class specifies only `in: List[Sym]` as the inputs of the block, and `res: Exp` as the output of the block. Let's ignore `ein` and `eff` for now, which will cover later.

We use `Block` in the `rhs` of `Node` to represent functions and control flow structures (conditionals, loops). Functions are declared using `"λ"` as the operator, and its `rhs` contains just a `Block` representing the function body. Note that recursive functions need an additional special node declared with `"λforward"`, which takes a `Sym` as function name and an integer `arity`. The node serves as a function name declaration. Conditionals are declared using `"?"` as the operator, and its `rhs` contains three items: a condition `Exp`, an if-branch `Block`, and an else-branch `Block`. While-loops are declared using `"W"` as the operator. It contains two blocks representing the loop condition and the loop body. We also have `Switch`, similar to the `switch...case` statement in C. The operands of a switch statement contain a `guard` representing the condition of the switch statement and a list of `Block`s for each case.

As we mentioned before, a `Block` only specifies its inputs and result. Where are the nodes that should go into the block? It turns out that the nodes are in the graph but not explicitly scoped in the block. When we traverse the graph for analysis, transformation, or code generation, the nodes
of each block are dynamically selected from the set of all available nodes using certain criteria. We call this process *scheduling*.
This is a key feature of *sea-of-nodes* styled IR, which is covered in more details later.

One may wonder why we use a sea-of-nodes IR, where dynamically scheduling the nodes seems to be a compilation overhead. The advantage is that since nodes are not fixed to a scope, they can be easily moved in/out of scopes, which can be a powerful optimization (called code motion). For instance, a user might construct a graph like this:

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

We can achieve the same optimization using an expensive dataflow analysis on a tree-based IR. But you will see in later sections that our approach is more lightweight and easier to implement.

### Data and Control Dependencies

From the previous example, hopefully the reader has gained a basic understanding of why we use sea-of-nodes IR. We briefly mentioned that the nodes are scheduled into blocks using certain criteria. What are these criteria? 

The criteria we use in LMS IR are *dependencies*. Simply speaking, dependencies capture information flow in the source program that we must respect. Given the order of nodes in the source program defined by the programmer, we can see that certain nodes can be reordered or moved in/out of scopes without changing the semantics of the program, but certainly, that's not true for all nodes. Dependency information tells us the order of nodes we must keep when we schedule nodes. There are two kinds of dependencies that we care about in LMS IR:
1. the data dependency and
2. the control dependencies.

The data dependency is straightforward. If a node uses an `Exp`, then the node
depends on that `Exp` and the node that generates that `Exp`. Data dependency is captured explicitly by our definition of `Node`. If the `rhs` of node A contains node B, then A is data-dependent on B. For instance, in the following code snippet:

``` scala
val x1 = 10 + x0
val x2 = x1 + 100
```
Node `x2` is data-dependent on node `x1` because it uses `x1` for computation. 

Data dependency also occurs between nodes and blocks. Since each block
has inputs `in: List[Sym]` and result `res: Exp`, we know that if `res` depends on node A, A must
be scheduled inside or before the block. In addition, if node A depends on the inputs, it must be scheduled inside the block (if the block needs it). For example:

``` scala
val x1 = 10 + x0
val x2 = x1 + x3
Block([x3], x2, _, _)
```
This snippet means that the block returns `x2` as a result, thus node `x2` and node `x1`
must be scheduled in or before the block. Also, since node `x2` is data-dependent on `x3`, which is
an input symbol of the block, `x2` must be scheduled inside the block. So we have two possible ways of scheduling this block:

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

We can choose the more optimized version during graph traversal.

Now let's consider control dependencies. What are control dependencies? Instead of offering a definition, let's enumerate some examples:

1. If we have two print statements in the IR, we cannot generate code with any one of them missing or in the wrong order.
2. If we have a mutable variable or a mutable array, the reading and writing of the mutable elements need to be controlled carefully. Some of the reading and writing can be removed or reordered, but certainly not all of them.
3. more ???

Let's see some examples:

``` scala
var x = 0
x = 3
x = 10
print(x)
```

In this example, the node `x = 10` is not *data-depended* by the print statement.
However, it is *control-depended* because the value of `x` is mutated by it. However,
the `x = 3` node can be removed without breaking the code semantics. The order of
graph construction (the fact that the user did `x = 3` before `x = 10`) is the base of our control dependency analysis.

### Effects and EffectSummary

How can we obtain data and control dependency information from the source program? As we mentioned before, data dependency is already captured by the definition of `Node`. However, it is not clear how to collect control dependencies. It turns out that we can track the *effects* of nodes and blocks and then compute control dependencies from these effects. To make our analysis more fine-grained (so that more interesting code reordering and optimization can be introduced), the first step is to realize that there are different kinds of effects. The effect of printing is clearly not so related to the effect of variable reading. Thus there should be no scheduling enforcement between them.
In LMS IR, we track *read* and *write* effects. They are categorized in the following cases:

1. Statements that create mutable objects belong to the category keyed by `STORE`.
2. Statements that read/write a mutable object belong to the category keyed by the symbol of this object (result of an allocation node).
3. For all remaining effectful statements (such as `printf`), they belong to the category keyed by `CTRL` (for control flow dependent). 

We track the effects for each node and block using `EffectSummary`, as shown in the definition of `Node` and `Block`. The definition of `EffectSummary` is the following:
```scala
case class EffectSummary(sdeps: Set[Sym], hdeps: Set[Sym], rkeys: Set[Exp], wkeys: Set[Exp])
```
The `rkeys` and `wkeys` track read and write effects, while `sdeps` and `hdeps` track soft and hard dependencies. To help with debugging, we also provide a string representation for `EffectSummary`: We use `[key: sdeps | hdeps]` where write-keys are prefixed with a `*`. For example, `[CTRL*: _ | x0]` means that the node has a write effect on key `CTRL`; it has no soft dependency and has a hard dependency on `x0`.


Besides `EffectSummary`, there is another effect-related field in our IR: the `ein: Sym` of `case class Block`. It stands for `effect-input` of a block. Previously, we
talked about normal inputs of blocks `in: List[Sym]`, and we said that "if a node depends
on `in`, then it must be scheduled inside the block, if ever needed". Similarly, if a node depends
on `ein`, then it must also be scheduled inside the block, if ever needed. However, `ein` is not
a real input. It is just an effect-input used to control the scheduling of
nodes when a node must be in a block by control dependencies. For instance:

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

### From Effects to Dependencies

After collecting effects, we need to compute dependencies from them. The
rules for computing dependencies from effects are listed below:

1. Read-After-Write (RAW): there should be a hard dependency from R to W,
   since the correctness of the R depends on the W to be scheduled)
2. Write-After-Read (WAR): there should be a soft dependency from W to R,
  since the correctness of the W does not depend on the R to be scheduled, but the order of them matters.
3. Write-After-Write (WAW): the first idea is to generate a soft dependency,
  since the second W simply overwrites the first W.
  However, write to array is more complicated, such as arr(0) = 1; arr(1) = 2,
  where the second W doesn’t overwrite the first W, and both Ws have to be generated.
  For now, we just issue a hard dependency from the second W to the first W.
4. Read-After-Read (RAR): there is no effect dependency between them.

Note that we introduced soft dependencies in the rules.
Soft dependencies are soft in the sense that, if node A is soft-dependent on node B,
node B cannot be scheduled after A. However, scheduling A does not ensure that B is scheduled.
But if A is hard-dependent on B, then scheduling A implies that B has to be scheduled before A.


### Dependencies and Scheduling

How do dependencies help with scoping the nodes in a block? The details are in `lms/core/traversal.scala`, but we can talk about it in the high level for now. Here are
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

#### Const Effects

The STORE and CTRL keys are also handled in our read/write system in a case-by-case manner.
For instances, allocating heap memory can be modeled as a read on the STORE key,
if we don’t care about the order of heap allocation.
However, printf should be modeled as a write on the CTRL key, since all prints should be
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
structures such as arrays, lists, etc. The current approach in LMS_clean doesn't support
higher-order functions. LMS application using higher-order functions in LMS IR will trigger errors in
Latent Effect analysis.

#### Effects in Data Structures

LMS IR supports `Array`, which is a mutable data structure holding a fixed number of same typed elements
in consecutive memory. The effects on arrays are at the whole-array granularity (reading and writing to
an element is treated as reading and writing to the whole array). This is less ideal.

LMS IR also supports other data strucutures (including `List`, `Tuple`, `Map`, etc). These data
structures are immutable, pure data structures, thus no effects are needed to use them.

#### Watch Out (Silent Failures in Effect System)

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


# Constructing LMS Graph

In this section, we will talk about how an LMS Graph is constructed using the LMS IR components. It shows how the LMS IRs are used in constructing LMS Graphs, and how effects and dependencies are tracked and generated.
All LMS snippets are functions. As a result, all LMS Graphs have a list of nodes
(already in topological order)
and a block describing the function. That is captured by the definition of `Graph`:

``` scala
case class Graph(val nodes: Seq[Node], val block: Block, val globalDefsCache: immutable.Map[Sym,Node])
```

in `lms/core/backend.scala`. The LMS Graph is constructed by `class GraphBuilder` in `lms/core/backend.scala`.

Firstly, our `GraphBuilder` class uses the following data structure to store nodes and symbol information:
* `globalDefs`: the list of all `Node`s in the graph.
* `globalDefsCache`: the map from `Sym`s to `Node`s.
* `globalDefsReverseCache`: the map from operator and RHS definitions to `Node`s. This is useful for implementing common subexpression elimination (CSE) of pure nodes. 

The two `findDefinition` functions query `globalDefs` or `globalDefsReverseCache` to find a `Node` given a `Sym` or a tuple of an operator and RHS list.

`GraphBuilder` also has a `fresh` function that returns a fresh integer for identifying a new symbol.

`GraphBuilder` also provides a `rewrite` function for pre-construction optimization. This function is not implemented in the base class and can be implemented for child classes of `GraphBuilder`.

To facilitate our implementation, we also use the following data structure to track effect-related content. Togetherly, they are called effect environments:
* `curLocalReads` and `curLocalWrites`: set of all read/write keys of the current block
* `curEffects`: a map of type: `Exp -> (Sym, List[Sym])`, which tracks the *last write* and the *reads after last write* for each Exp key.

Besides all these, `GraphBuilder` offers two key functionalities:

1. Building nodes by the @reflect*@ family of methods.
2. Building blocks by the @reify*@ family of methods.

### Adding Nodes using *reflect* functions
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
This method takes four inputs of an expression: the operator as a `String`, the RHS as one or many `Def`s, the read effect keys `readEfKeys` and the write effect keys `writeEfKeys`.

This method first tries to run a pre-construction optimization via `rewrite`, which checks if the node to be constructed can be optimized by using a pure `Exp` already present in the graph. If so, we just return the `Exp` directly. Otherwise, we build a new node in the `None` case.

For the latter case, we first need to gather all read and write effects of a node. The input effects `readEfKeys` and `writeEfKeys` alone are not sufficient because nodes like functions have latent effects. Therefore, we first compute the latent read and write effects via `getLatentEffect` helper method,
and combine the results with the user-provided `readEfkeys` and `writeEfKeys`. We will discuss `getLatentEffect` in greater detail later.

If the effect keys are empty, then we determine that the node is pure and go to the else-branch. In this case, we attempt to perform CSE via the `findDefinition` helper methods. If we can find a previously added node that has the same operator and RHS, we just return the symbol of that node. Otherwise, we call `reflect` method with no `EffectSummary` arguments (since it's a pure node). The `reflect` method will build a new Node, add it into the global maps, and return it as the result.

If the effect keys are not empty, we create a fresh new symbol `sm` for the new node. Then, we compute dependencies from the effect keys via `gatherEffectDeps` method, which will also be discussed later. We build a new `EffectSummary` from the gathered dependencies and call `reflect` to update the global maps. 

The last thing to do is to update the effect environment. We add read keys to `curLocalReads` and write keys to `curLocalWrites`. Then, we need to update `curEffects`. For all read keys of the new node, we update the `curEffects` of the read keys by appending the new node to their las reads. Similarly, for all write keys of the new node, we update their values in `curEffects` such that their last write becomes the new node `res`, and their last reads are cleared. Finally, we return the new node `res`.

### *getLatentEffect*
We now discuss the `getLatentEffect` family of methods. They recursively call each other to dig out all latent effects of Blocks, including lambdas, conditionals, and blocks. The entry point of these methods is the following:

```scala
def getLatentEffect(op: String, xs: Def*): (Set[Exp], Set[Exp]) = (op, xs) match {
  case ("λ", _) => (Set[Exp](), Set[Exp]())
  case ("@", (f: Sym)+:args) => getApplyLatentEffect(f, args:_*)._1
  case _ => getLatentEffect(xs:_*)
}
```
This function takes an operator and a RHS of an expression as inputs. It returns the read and write keys of this expression as two sets. If we see a function declaration (`"λ"`), we return empty sets because a function declaration itself has no effects. The effects for functions are handled by their call site (applications), which is handled by `getApplyLatentEffect`. For all other cases, we go to this method:
```scala
def getLatentEffect(xs: Def*): (Set[Exp], Set[Exp]) =
  xs.foldLeft((Set[Exp](), Set[Exp]())) { case ((r, w), x) =>
    val (ref, wef) = getLatentEffect(x)
    (r ++ ref, w ++ wef)
}
```
which is a wrapper over `getLatentEffect(x)` that accumulates effects of multiple Defs:

``` scala
def getLatentEffect(x: Def): (Set[Exp], Set[Exp]) = x match {
  case b: Block => getEffKeys(b)
  case s: Sym => findDefinition(s) match {
      case Some(Node(_, "λ", (b@Block(ins, out, ein, eout))::_, _)) => getEffKeys(b)
      case _ => (Set[Exp](), Set[Exp]())
  }
  case _ => (Set[Exp](), Set[Exp]())
}
```
Given a `Def`, this function collects the latent read keys and write keys. If we see a `Block`, then we just return the effect keys of the block by the helper function `getEffKeys`. If we see a `Sym`, we first find the `Node` of that `Sym` by helper method `findDefinition`. If the returned node is a function (lambda node), we just return the effect keys of the function body block. For all other nodes, we just return two empty sets. 

Now let's go back to `getApplyLatentEffect`. The implementation of which is the following:
```scala
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
```
This method takes the function name and arguments as input. It first collects the latent effects of the arguments. Then, we call helper `getFunctionLatentEffect`, which returns 5 items: the `freads` and `fwrites`, which are read/write keys of the function excluding parameters, the `preads` and `pwrites`, which are read/write keys of the function parameters, and `res` which is the result of the function body. We need `res` because it can be another function that also has latent effects. Lastly, we replace effects on function parameters to the actual arguments. 

Now let's discuss `getFunctionLatentEffect`:
```scala
def getFunctionLatentEffect(f: Exp): ((Set[Exp], Set[Exp]),(Set[Int], Set[Int]), Option[Exp]) =
  findDefinition(f) match {
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
    case Some(Node(_, "module", (b:Block)::_, _)) =>
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
```
The method takes an `Exp`, which should be evaluated to a lambda (or lambda forward).
It returns ((read_keys, write_keys), (read_parameters, write_parameters), result)
             Set[Exp]   Set[Exp]      Set[Int]: index  Set[Int]: index    Option[Exp]
1. read_keys/write_keys: effect keys of the function (excluding effects to parameters)
2. read_parameters/write_parameters: effects of the function to its parameters (just returning indices, not Exps). Using Set[Int] (index) for read_parameters and write_parameters is necessary because in the conditional case, the parameters may have different names (symbols) and cannot be Unioned. the indices can be unioned easily
3. result: the result of the function body (useful if the result is another function that has latent effects). it is Option[Exp] because in some cases I am not sure what result to return.

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

### *gatherEffectDeps*
Now, we discuss the `gatherEffectDeps` method used in `reflectEffect`. This method computes dependencies from effect keys:

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
// Conservative handling of dependencies for write keys: (return soft deps and hard deps respectively)
// 1) write has hard dependencies on previous write (this is conservative for array case, Store, Ctrl,...)
// 2) write has soft dependencies on previous read (just enforcing order, do not enforcing the reads to be scheduled)
def gatherEffectDepsWrite(s: String, as: Seq[Def], lw: Sym, lr: Seq[Sym]): (Set[Sym], Set[Sym]) =
  (if (!reflectHere) lr.toSet else Set(), Set(latest(lw))) // FIXME(feiw) why not adding soft dependencies when reflectHere?
```
Given read keys, write keys, operator and RHS, the method returns hdeps and sdeps as two sets. The method first initializes two lists `prevHard` and `prevSoft`. Then, it traverses all write keys in the input. We check if each write key can be found in the current effect environment. If so, we call helper method `gatherEffectDepsWrite` and accumulates the returned sdeps and hdeps to `prevSoft` and `prevHard`, respectively. The helper method performs a convervative handling of dependencies for write keys. We enforce WAW as a hard dependency, which is conservative for arrays, STORE, CTRL, etc. We enforce WAR as a soft dependency and do not require the reads to be scheduled.

if the write key is not in the current effect environment, then the write key has a hard dependency on declaration (if declared locally) or block (if declared globally, i.e., out of current block). We call helper method `latest` to return the local `Sym` or the `curBlock`, respectively.

We then handle `reifyHere`, which s an Effect Optimization for conditional and switch. It allows the block of conditionals to be aware of the `curEffects` out of the block. 

Lastly, we handle read keys by adding the last writes of each key to `prevHard`, and return `prevHard` and `prevSoft`.

### Adding Blocks using *reify* functions
Now we discuss how to add blocks into the Graph by the reify*` family of methods.
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

# Dead Code Elimination Phase
In this section, we discuss `DeadCodeElimCG`, which is defined as Graph-to-Graph transformation (`Phase`). It removes all nodes not used in computing the result. The pass is implemented in three steps: 1) collect liveness and reachability information, 2) remove unused variables, and 3) reconstruct the graph.

### Collect Liveness and Reachability Information
First, we need to make a distinction between liveness and reachability. A node is *reachable* if it can be backtraced from the block result via hard dependencies, which can be data dependencies or hard effect dependencies. A node is *live* if its value is needed (only data dependency). Reachability and liveness are orthogonal. For instance, a conditional node can have side effects and return values. If the side effects are relevant, then the conditional node is reachable. If the return values are relevant, the conditional node is live. We use two sets `live` and `reach` to track liveness and reachability, respectively. We also use a set `used` to track all used variables. The logic for calculating liveness and reachability is the following:

```scala
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

      newNodes = nn::newNodes
    }
  }
}
```
We first add the top-most block node of the graph to `reach`. If the block returns a `Sym`, we add that symbol to both `live` and `used`. We then add the effect and data input symbols to `used`. Then, we traverse all nodes in the input graph in reverse order. If a node is reachable, we add the `valueSyms` of that node to `live`, and `hardSyms` to `reach`. The `valueSym` collects all used values of a node, including the symbols and block inputs of the RHS. The `hardSym` collects hard dependencies of a node. Lastly, we add the current reachable node to `newNodes`.

### Remove Unused Variables
```scala
for (d <- newNodes.reverseIterator) {
  if (used(d.n)) {
    used ++= valueSyms(d)
  } else if (d.eff.hasSimpleEffect || d.eff.wkeys.exists(used)) {
    used += d.n
    used ++= valueSyms(d)
  }
}
```
After getting all reachable nodes in `newNodes`, we continue collect used variables by traversing `newNodes` in reverse order. For each node `d`, if the symbol of `d` is in `used`, we add the `valueSym` of `d` into `used` as well. If `d` is not in `used` but has simple effects (e.g. print) or writes to a used symbol, we also add the symbol and `valueSym` of `d` into `used`.

### Recreate the Graph
```scala
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
```
Lastly, we recreate the graph based on the information we collected earlier. We only consider nodes in `newNodes` that are also in `used`. For each such node, we update `newGlobalDefsCache` to map the symbol of the node to the node itself. We then remove unused effect symbols from the RHS and `EffectSummary` of the node. We do the same thing to the top-most block and return a new `Graph` with the updated information.
