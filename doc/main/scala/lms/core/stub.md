## Fully Fledged Frontend

After discussing the simple frontend in [LMS Simple Frontend](frontend.md), we want to show
what a normal frontend looks like.

The fully fledged frontend uses an over-arching object to handle graph construction.
(in the object Adaptor at lms/core/stub.scala).

``` scala
object Adapter extends Frontend {
   var typeMap // map backend Exp to Rep[T]
   var funTable // used to cache scala functions for `reflect "lambda"`

   val g = makeGraphBuilder() // graph builder
   def emitCommon // code gen
}
```

The graph construction depends on various supports including the basic support for `Rep[T]`,
control flows, Array, List, et al. All of them are defined as `Trait` and can be mixed in
for specific use case. Any frontend extension (such as supporting new frontend types and
constructions) should also implement them as `Trait`s.

### Base Trait: Introducing Rep[T]

In the Base trait, the code establish the iconic Rep[T] of LMS.
Previously in Frontend class, we have seen one way to wrap around
`core.backend.Exp` so that we can construct LMS Graph via unary operators,
binary operators, and et al. What is to be further provided in Base trait
is the ability to use `Rep[T]`. Similarly `Rep[T]` is built on top of
`core.backend.Exp`. The `core.backend.Exp` do not have types. The types are
added via user code and type inferencing, and then tracked in a data structure
called `typeMap` (such as `Adaptor.typeMap`).

``` scala
trait Base extends EmbeddedControls with OverloadHack with ClosureCompare {
    type Rep[+T] = Exp[T]  // type name aliasing

    abstract class Exp[+T] // track LMS IR for non-variables
    abstract class Var[T]  // track LMS IR for variables

    // The Wrap class and method that build Rep[T] typed expression with type tracking
    case class Wrap[+A:Manifest](x: lms.core.Backend.Exp) extends Exp[A] {
        Adapter.typeMap(x) = manifest[A]
    }
    def Wrap[A:Manifest](x: lms.core.Backend.Exp): Exp[A] = {
        if (manifest[A] == manifest[Unit]) Const(()).asInstanceOf[Exp[A]]
        else new Wrap[A](x)
    }

    // The WrapV class for Var[T]
    case class WrapV[A:Manifest](x: lms.core.Backend.Exp) extends Var[A] {
        Adapter.typeMap(x) = manifest[A]
    }
}
```

Generally speaking, all `lms.core.Backend.Exp` are wrapped in `case clas Wrap[T]`
that is of type `Exp[T]` (aliased as `Rep[T]`). The type mapping from `lms.core.Backend.Exp`
to type manifest is maintained in `typeMap`.


### Base Trait: Better Handling of Functions

In the simple frontend, we see that the handling of recursive functions is
a bit awkward (the user code has to explicitly handle the function itself).
How do we make it better in Base Trait? In Base trait, we support the *lifting*
of any `Rep[A] => Rep[B]` function to `Rep[A => B]` (staged) functions.
This is done by the `__fun` function.

``` scala
def fun[A:Manifest, B:Manifest](f: Rep[A]=>Rep[B]): Rep[A=>B] =
    Wrap[A=>B](__fun(f, 1, xn => Unwrap(f(Wrap[A](xn(0))))))
```

Unfortunately, we have to implement multiple `fun`s for different function
arities, which we will elide here. The `__fun` function reifies the argument
into a function block. How does this `__fun` work? The key is the `canonicalize` function,
which returns a unique string representation of a scala function. It ensures that
each function will be refied at most once. For any invocation of the function that is
not the first invocation, it will return the cached `Backend.Sym` directly.
If the non-first invocation happens after reify finishes, the returned `Backend.Sym`
is the `Sym` that binds to the `reflect("lambda")`. However, if the non-first
invocation happens before the reify finishes (such as in the case of recursive functions),
the returned `Backend.Sym` is the `Sym` that binds to the `reflect("lambda-forward")`,
which serves as the *forward-reference* of the function. This forward reference is necessary
not only for C/C++ backend, but also for Scala backend (that generates Scala code),
because not all Scala code allows referencing a function defined later (see the Atomaton test
in LMS tutorial).

``` scala
def __fun[T:Manifest](f: AnyRef, arity: Int, gf: List[Backend.Exp] => Backend.Exp): Backend.Exp = {
    // use canonicalize to get the unique representation of any Scala function
    val can = canonicalize(f)
    Adapter.funTable.find(_._2 == can) match {
        case Some((funSym, _)) =>
            funSym // Easy case: found the function in funTable
        case _ =>
            // Step 1. set up "lambdaforward" node with 2 new fresh Syms
            val fn = Backend.Sym(Adapter.g.fresh)
            val fn1 = Backend.Sym(Adapter.g.fresh)
            Adapter.g.reflect(fn, "lambdaforward", fn1)()

            // Step 2. register (fn, can) in funTable, so that recursive calls
            //    will find fn as the function Sym. Reify the block.
            // Note: it might seem strange why/how recursive calls re-enter this __fun() function.
            //    The reason is that in user code, recursive functions have to be written as
            //    lazy val f = fun{...} or def f = fun{...}, in which case the recursive calls
            //    will re-enter the `fun` call.
            Adapter.funTable = (fn, can)::Adapter.funTable
            val block = Adapter.g.reify(arity, gf)

            // Step 3. build the "lambda" node with fn1 as the function name
            //    fix the funTable such that it pairs (fn1, can) for non-recursive uses.
            val res = Adapter.g.reflect(fn1,"lambda",block)(hardSummary(fn))
            Adapter.funTable = Adapter.funTable.map {
            case (fn2, can2) => if (can == can2) (fn1, can) else (fn2, can2)
            }
            res
    }
}
```

Although the `__fun` function provided a nice solution to recursive functions, it does
add complexity to code generations and LMS graph transformations due to the `lambda-forward` node,
which we will cover later.
The Base trait also provides `topFun`, which is a variant of `fun` that is supposed to be lifted
to top level (for C code generation). There are still many limitations to topFun related to closure
conversion, recursion, and so on.

The Base trait also provide macro support for native `if`, `while`, and `var`, by extending
the `EmbeddedControls` and providing the following methods:

``` scala
def __ifThenElse[T:Manifest](c: Rep[Boolean], a: => Rep[T], b: => Rep[T]): Rep[T]
def __whileDo(c: => Rep[Boolean], b: => Rep[Unit]): Rep[Unit]
def var_new[T:Manifest](x: Rep[T]): Var[T]
def __assign[T:Manifest](lhs: Var[T], rhs: T): Unit
```

With the macro (`@virtualize`), native `if`, `while`, and `var` are syntactically transformed
to these functions, which are then converted to `IF`, `WHILE`, and `WrapV[T]`.
The Base trait also support misc ops, boolean ops, timing ops, comment ops, unchecked ops, and
et al.

###  Rep[T] Methods

Since a Type `T` object can have many methods, we need to support those methods for Type `Rep[T]`
objects. The way is to implement

1. implicit conversion from T, Rep[T], and Var[T] to a class (TOpsCls) wrapping Rep[T].
2. all the desired methods in that class (TOpsCls).

An example of ordering is given below. All these traits have to extend `Base` or be implemented
in `Base` trait directly, to be able to use `Rep[T]`. This pattern of code is used for many types
that are supported in LMS IR as `Rep[T]`, including primitives (such as Int, Float, and Double) and
some data structures (such as List, Tuple, Array, and Map)

``` scala
trait OrderingOps extends Base with OverloadHack {
  implicit def orderingToOrderingOps[T:Ordering:Manifest](n: T) = new OrderingOpsCls(unit(n))
  implicit def repOrderingToOrderingOps[T:Ordering:Manifest](n: Rep[T]) = new OrderingOpsCls(n)
  implicit def varOrderingToOrderingOps[T:Ordering:Manifest](n: Var[T]) = new OrderingOpsCls(readVar(n))

  class OrderingOpsCls[T:Ordering:Manifest](lhs: Rep[T]){
    def <  (rhs: Rep[T])(implicit pos: SourceContext) =
      Wrap[Boolean](Adapter.g.reflect("<", Unwrap(lhs), Unwrap(rhs)))
    def <= (rhs: Rep[T])(implicit pos: SourceContext) =
      Wrap[Boolean](Adapter.g.reflect("<=", Unwrap(lhs), Unwrap(rhs)))
  }
```

