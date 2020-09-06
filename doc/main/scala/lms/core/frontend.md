## Simple Frontend

With a proper definition of LMS IR and the facility to build LMS Graph in lms/core/backend.scala,
we can build a frontend that construct LMS Graph. The LMS frontend should feature the `Rep[T]`
type, which allows the staged programs to be type checked. However, in this section, we are
going to introduce a very simple frontend that can just nit the LMS Graph, without the iconic
`Rep[T]` type. This simple frontend is not of much use in production, but it shows the simple
essence of LMS front, i.e., being able to construct LMS Graph with various control flows.

The basic ways to construct LMS graphs are through the `reflect*` and `reify*` family of
functions. However, the frontend should allow the users to construct LMS graphs via unary
and binary operations, `If`, `While`, `FUN`, and et al. That is the main purpose of the
simple FrontEnd class.

``` scala
class FrontEnd {

    var g: GraphBuilder = null // LMS graph is built in here

    case class BOOL(x: Exp) // boolean wrapper of LMS EXP, supporting ! op
    case class INT(x: Exp)  // int-like wrapper of LMS EXP, supporting arithmetic op
    case class ARRAY(x: Exp) // array wrapper of LMS Exp, supporting array access.
    case class VAR(x: Exp) // variable wrapper of LMS Exp, supporting variables

    // supporting conditional. a and b are executed in reify*.
    // then the returned blocks are used in reflect* to build the conditional node
    def IF(c: BOOL)(a: => INT)(b: => INT): INT = {...}

    // supporting loop. both c and b are executed in reify*,
    // then the returned blocks are used in reflect* to build the loop node
    def WHILE(c: => BOOL)(b: => Unit): Unit = {...}

    // supporting application. just creating an app node.
    def APP(f: Exp, x: INT): INT = INT(g.reflect("@", f, x.x))

    // If we just need to support in-graph, non-recursive functions, this is enough.
    // It builds a lambda node with the `f' reified into the block,
    // then it returns a scala function that can be applied to create APP construct
    def FUN(f: INT => INT): INT => INT = {
      val fn = g.reflect("lambda", g.reify {xn: Exp => f(INT(xn)).x})
      (x: INT) => APP(fn, x)
    }
}
```

It would be interesting to find out how to achieve in-graph, recursive functions.
If we want to support recursive functions, we have to create a lambda node that
uses the lambda Exp within the lambda block.
``` scala
val fn = Sym(g.fresh)
g.reflect(fn, "lambda", g.reify(???))
```

In order to be able to use the same `fn` in the block of the lambda, we need to be
able to construct `APP(fn, xn)` within the `g.reify`.
``` scala
val f1 = (x: INT) => APP(fn, x)
```

But we don't know how the `f1` is recursively used. That has to come from user code `f`.
``` scala
def FUN(f) = {
    ...
    g.reflect(fn,"lambda",g.reify(xn: Exp => f(f1, INT(xn)).x))()
}                                       ^^^^
// user code `f' decides how `f1' (the in-graph lambda) is recursively used.
```

To put everything together and offer an optional non-recursive API:
``` scala
def FUN(f: INT => INT): INT => INT = FUN((_,x) => f(x))

def FUN(f: ((INT=>INT),INT) => INT): INT => INT = {
    val fn = Sym(g.fresh)
    val f1 = (x: INT) => APP(fn,x)
    g.reflect(fn,"lambda",g.reify(xn => f(f1,INT(xn)).x))()
    f1
}
```

A use case might be
``` scala
val fac = FUN { (f, n) =>
    IF (n !== 0) {
        n * f(n-1) // recursive call
    } {
        1
    }
}
```

Finally, the `program` function reifies the user provided snippet (of type `INT => INT`)
and returns the LMS Graph.
