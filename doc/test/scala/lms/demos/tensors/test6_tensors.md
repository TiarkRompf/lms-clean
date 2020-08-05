## Tensor Transformation

Although written in test folder, this demon is showing a way to build `Tensor` types based on the very simple `Frontend` in Frontend.scala.
Remember that in `Frontend`, we provided a way to nit LMS IR graphs without using
the iconic `Rep[T]`. Instead, we just used `case classes` to hold `Backend.Exp`s.
We had basic things like:

``` scala
case class BOOL(x: Exp)
case class INT(x: Exp)
case class ARRAY(x: Exp)
case class VAR(x: Exp)
def IF(c: BOOL)(a: => INT)(b: => INT): INT
def WHILE(c: => BOOL)(b: => Unit): Unit
def APP(f: Exp, x: INT): INT = INT(g.reflect("@", f, x.x))
def FUN(f: INT => INT): INT => INT = FUN((_,x) => f(x))
def FUN(f: ((INT=>INT),INT) => INT): INT => INT
```

This file (test6_tensors.scala) adds more frontends

``` scala
case class SEQ(x: Exp)
case class Tensor(x: Exp)
...
```

and several more. Without seeing the code gen part of the nodes, we are
not very sure of the intended semantics of this frontend. However, it is
presumably true that they are for `sequence` and `tensor`.

But this file is not just adding more frontends. The most important aspect
of this file is to introduce use-cases of transformation. If you have not yet
read the [LMS Transformation](../../../../../main/scala/lms/core/traversal.md), please do.

Then we will see some important example of transformation for tensor computation optimizations.

### Tensor Computation Lowering

One example of such transformation is for lowering, not optimization. It is done like this:

``` scala
  override def transform(n: Node): Exp = n match {
    case Node(s, "tensor_zeros", List(sh:Exp), _) => Tensor(SEQ(transform(sh)))(is => 0).x
    case Node(s, "tensor_ones",  List(sh:Exp), _) => Tensor(SEQ(transform(sh)))(is => 1).x
    case Node(s, "tensor_add",  List(a:Exp,b:Exp), _) =>
      val (a1,b1) = (Tensor(transform(a)), Tensor(transform(b)))
      val shape = same_shape(a1.shape, b1.shape)
      Tensor(shape)(is => a1(is) + b1(is)).x
    case _ => super.transform(n)
  }
```

Basically, it lowers three high-level nodes ("tensor_zeros", "tensor_ones", and "tensor_add") to
low-level ops of Tensor construction (via `Tensor(shape)(fun)`). Doing so allows the three high-level
nodes to be handled without doing so in codegen.

### Tensor Fusion

The tensor fusion transformation show-cases the fusion of tensor creation and tensor application.
It is likely overzealous to do so but it is good to see that we can do so.

``` scala
val tensors = new mutable.HashMap[Sym, (Node, List[Sym], Seq[Node])]

// NOTE: fuse _all_ tensors expressions vertically
// (produce/consume) if possible. This is quite likely overzealous.
override def transform(n: Node): Exp = n match {
    case Node(s, "tensor", List(sh:Exp, f:Block), _) =>
        tensors(s) = (n, path, inner)
        super.transform(n)
    case Node(s, "tensor_apply", List(a:Sym,b:Exp), _) if tensors contains a =>
        val (Node(_, _, List(sh:Exp, f @ Block(arg::Nil, res, block, eff)), _), path0, inner0) = tensors(a)
        if (subst contains arg) println(s"Warning: already have a subst for $arg")
        try {
            subst(arg) = transform(b)
            withResetScope(path0, inner0) { traverse(f) }
            transform(res)
        } finally subst -= arg
    case _ => super.transform(n)
}
```

The `tensors` hashmap is used to remember all tensor creation via node `Node(n, "tensor", shape, fun)`.
Then the transformation of tensor application of those tensors tries to apply the `fun` with the given index.

``` scala
subst(arg) = transform(b)
withResetScope(path0, inner0) { traverse(f) }
transform(res)
```

It should be noted that when traversing the block of `fun`, we need to use the old `inner` (list of
available nodes) stored in the `tensors` hashmap, but the new `path` that is no less than the `path`
stored in the `tensors` hashmap. Maybe as an exercise, think about why.

### Tensor Horizontal Fusion

The basic idea of tensor horizontal fusion is like this:

``` scala
println(tensor(List(3, 4, 5), x1 => 2))
println(tensor(List(3, 4, 5), x2 => 3))

>>>> FUSE TO >>>>

val x1 = multiloop(List(3, 4, 5), List(tensor, tensor), x2 => List(2, 3))
println(seq_apply(x1, 0))
println(seq_apply(x1, 1)
```

The code is highly preliminary, but it is good to check some of the key logics in the current implementation. The key part is to combine the tensor blocks of several "tensor" nodes into one
tensor block, if they have the same shape. The code to emit the fused body is:

``` scala
// emit the fused body ...
val loopResPos = new mutable.HashMap[Sym,Int] // local cse on loop results
val buf = new mutable.ListBuffer[(String,Exp)]
val newBody = this.g.reify { e =>
    for ((Node(s,op,List(sh:Exp,b@Block(arg::Nil, res, block, eff)), _),i) <- loops.zipWithIndex) yield {
        assert(transform(sh) == shape, "ERROR: fused loop shapes don't match (TODO!)")
        subst(arg) = e
        traverse(b)
        val r = transform(res)
        val i = buf.indexOf((op,r))
        if (i < 0) {
            loopResPos(s) = buf.length
            buf += ((op,r))
        } else loopResPos(s) = i
    }
    this.g.reflect("seq", buf.map(_._2).toList:_*)
}
val fusedLoopSym = this.g.reflect("multiloop", shape, Const(buf.map(_._1).toList), newBody)
// and now the individual loop bodies
for ((Node(s,_,_, _),i) <- loops.zipWithIndex) {
    subst(s) = this.g.reflect("seq_apply", fusedLoopSym, Const(loopResPos(s)))
}
```

Basically a `newBody` is reified, within which the all tensor blocks are traversed using the
`newBody`'s parameter as argument (via `subst(arg) = e`). After `traverse(b)`, the result is collected
into a `seq` node, as a sequence of results.
Then the `newBody` is used to construct a new node called `multiloop`, and the result of each tensor
block is linked to a `seq_apply` node to be fetched from the sequence result.

### MultiLoopLowering

Ofcourse building such `multiloop` nodes meaning that we should transform/lower them or handle
code genenration from them. Here we are introducing the lowering transformation. We bring up
several code snippets here:


This helper function unroles a dimension into a while loop
``` scala
def forloop(sz: INT)(f: INT => Unit) = {
    val loopVar = VAR(0)
    WHILE (loopVar() !== sz) {
        f(loopVar())
        loopVar() = loopVar() + 1
    }
}
```

This helper function unroles a multi-dimension tensor iteration into nested loops:
``` scala
def forloops(sz: List[INT])(f: List[INT] => Unit): Unit = sz match {
    case Nil => f(Nil)
    case s::sz => forloop(s) { i => forloops(sz) { is => f(i::is) } }
}
```

This function traverses the multi-dimension tensor and runs the logic for each tensor index.
Note that the anonymous function is unroles in the inner most loop scope.
``` scala
forloops(shape) { e =>
    subst(f.in.head) = SEQ(e:_*).x
    traverse(f)
    val resTuple = transform(f.res)
    for ((b,i) <- builders.zipWithIndex) {
        val res = g.reflect("seq_apply", resTuple, Const(i))
        b += (e,INT(res))
    }
}
```

More details are in the code.

### MultiLoopBuilderLowering

We can also try not to do so much in one step. The `MultiLoopBuilderLowering` class doesn't
unroll the loops, but creates `builder` classes and `foreach` classes instead. Here we show
the snippet of `foreach` node to highlight the difference.

``` scala
def foreach(sz: SEQ)(f: SEQ => Unit): Unit = {
    val b = g.reify{ e => f(SEQ(e)); Const() }
    val summary = g.getEffKeys(b)
    g.reflectEffectSummary("foreach", sh, b)(summary)
}
```
Note that the `foreach` takes a `List[Int]` as shape.

### MultiDimForeachLowering

Then we handle `foreach` nodes and `builder` nodes in this `MultiDimForeachLowering` class.
The high-dimensional `foreach` nodes is reduced to multiple single-direction `foreach` nodes
by this snippet:

``` scala
def foreach(sz: INT)(f: INT => Unit): Unit = {
    val b = g.reify{ e => f(INT(e)); Const() }
    val summary = g.getEffKeys(b)
    g.reflectEffectSummary("foreach", sz.x, b)(summary)
}

def forloops(sz: List[INT])(f: List[INT] => Unit): Unit = sz match {
    case Nil => f(Nil)
    case s::sz => foreach(s) { i => forloops(sz) { is => f(i::is) } }
}
```

For `builder` nodes, we transformed `tensor_builder_new` to `array_new`,
`tensor_builder_add` to `array_set` with index linearization (changing high dimension indices
to flat index), and `tensor_builder_get` to getting the corresponding array symbol.
The code is here:

``` scala
case Node(s, "tensor_builder_new", List(sh: Exp), _) =>
    builders(s) = n
    val INT(Const(dims:Int)) = SEQ(sh).length // TODO: proper error message
    val shape = SEQ(sh).explode(dims)
    g.reflectMutable("array_new", shape.reduce(_*_).x)

case Node(s, "tensor_builder_add", List(builder: Sym, i: Exp, x: Exp), _) =>
    val (Node(s, "tensor_builder_new", List(sh0: Exp), _)) = builders(builder)
    val sh = transform(sh0) // TODO: tensor_builder_shape
    val INT(Const(dims:Int)) = SEQ(sh).length // TODO: proper error message
    val shape = SEQ(sh).explode(dims)
    val idx = SEQ(transform(i)).explode(dims)
    def linearize(sh: List[INT], xs: List[INT]): INT = (sh,xs) match {
    case (s::Nil, x::Nil) => x
    case (s::sh, x::xs) => x * sh.reduce(_*_) + linearize(sh,xs)
    }
    g.reflectWrite("array_set", transform(builder), linearize(shape,idx).x, x)(transform(builder))

case Node(s, "tensor_builder_get", List(builder: Sym), _) =>
    val (Node(s, "tensor_builder_new", List(sh: Exp), _)) = builders(builder)
    transform(builder)
```

Then a test driver using all the transformation passes and two tests are shown at the end of the file.
