## Tensor High-level IR.

Tensor IR is an example of defining high-level IRs in LMS and using transformation passes to
lower/optimize/parallelize the high-level IRs to low-level IRs.

Before going into the design details, let's first recap the basic infrastructure we have in LMS,
and see if they need to be updated for our purpose. In backend.scala, we have
`object Backend`
that contains the basic building blocks of `Exp`, `Def`, `Node`, and `Block`.
We have a `class GraphBuilder` that implements the `reflect` and `reify` functions for
building a LMS IR using the basic blocks in `object Backend`. The construction happens in the
frontend.scala file, where the `program` function reifies the `Block` that is the user program:

``` scala
def program(f: INT => INT): Graph = program(g.reify { x => f(INT(x)).x })
def program(body: => Block): Graph = {
  assert(g == null)
  g = mkGraphBuilder()
  try {
    val block = body
    Graph(g.globalDefs, block, g.globalDefsCache.toMap)
  } finally {g = null}
}
```

The logic of this snippet should be clear.
1. A `GraphBuilder` is constructed (the `mkGraphBuilder()` can be override by other classes).
2. The `body` block is evaluted to `reflect` the nodes and `reify` the blocks into the `GraphBuilder`. Then a `Graph` object is returned containing all nodes, final block, and a hashmap from `Sym` to `Node` (for getting the definition of a `Sym`).
3. Then the `g` is reset to null.

In the more sofisticated frontend `stub.scala`, we use an `object Adapter` as the graph constructor.
It overrides the `mkGraphBuilder()` with `GraphBuilderOpt`, which has more optimizations at
node construction.
To offer a `Rep[T]` type for each `Backend.Exp`, the `Backend.Exp` are wrapped in a `Wrap[T]` class
``` scala
case class Wrap[+A:Manifest](x: lms.core.Backend.Exp) extends Exp[A] {
   Adapter.typeMap(x) = manifest[A]
}
```
And the mapping from `Backend.Exp` to `Manifest[_]` is kept in `Adapter.typeMap`.
`Adapter.funTable` is another `Adapter` state that maps from frontend functions to their
canonicalized forms. Note that after building the LMS IR in `Adapter`, `typeMap` and `funTable`
are still avaiable (they are not reset to null).

``` scala
def emitCommon(name: String, cg: ExtendedCodeGen, stream: java.io.PrintStream,
              verbose: Boolean = false, alt: Boolean = false, eff: Boolean = false)
             (m1: Manifest[_], m2: Manifest[_])(prog: => Block) = {
  typeMap = new scala.collection.mutable.HashMap[lms.core.Backend.Exp, Manifest[_]]()
  funTable = Nil

  var g: Graph = time("staging") { program(prog) }
  ...
  ...
  time("codegen") {
    cg.typeMap = typeMap
    cg.stream = stream
    cg.emitAll(g,name)(m1,m2)
    cg.extractAllStatics
  }
}
```
The `Adapter.typeMap` is then passed to `cg: ExtendedCodeGen` for later uses (in code generation). So it seems that all important graph information is passed to codegen (the
graph, including the nodes, the last block, the hashmap from `Sym` to `Node`, and the
`typeMap` from `Exp` to `Manifest[_]`).

Then in C_codegen, the snippet to run codegen is

``` scala
def run(name: String, g: Graph) = {
  capture { // collect all emit to the stream of the code generator
    graphCache = g.globalDefsCache // pass the Sym->Node map to code generator
    bound(g) // run bound(g) to compute the bound variables (needed for traversal)
    withScope(Nil, g.nodes) { // initialize `path` to `Nil` and `inner` to `g.nodes` (all nodes)
      emitFunction(name, g.block) // quote the block: emit code for the block
    }
    graphCache = null // reset graphCache to null
  }
}
```

