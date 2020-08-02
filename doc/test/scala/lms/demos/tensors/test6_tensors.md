Although written in test folder, this demon is showing a way be build `Tensor` types based on the very simple `Frontend` in Frontend.scala.
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

To be continued.

