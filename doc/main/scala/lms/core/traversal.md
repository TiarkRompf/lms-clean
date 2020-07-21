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

### Dead Code Elimination

In a real scenario, we first run a dead code elimination (DCE) pass before we traverse the graph.
The DCE pass is currently implemented in lms/core/backend.scala, `class DeadCodeElimCG`.

The basic idea for DCE is like this. We track 2 categories of `Backend.Sym`s, the *live* and *reach* sets.
1. The reach set is the set of nodes (or Syms of the nodes) that are reachable via hard-dependencies.
It includes both data dependencies and control dependencies.
2. The live set is the set of nodes (or Syms of the nodes) that are needed only via data dependencies.

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

TO BE continued...
