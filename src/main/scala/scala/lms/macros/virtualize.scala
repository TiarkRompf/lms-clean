package scala.lms.macros

//import language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

import scala.util.matching.Regex

import scala.collection.mutable


object virtualize {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    /* Create a transformer for virtualization. */
    val transformer = new Virtualizer[c.type](c)

    /* The first element of `annottee` is the one actually carrying the
     * annotation.  The rest are owners/companions (class, method,
     * object, etc.), and we don't want to virtualize them.
     *
     * Also, for now, we don't virtualize annotated type, class or
     * method parameters (this may change in the future).
     */
    val inputs = annottees.map(_.tree).toList
    val (annottee, rest) = inputs match {
      case (a: ValDef) :: as => {
        c.warning(c.enclosingPosition,
          "virtualization of parameters is not supported.")
        (None, inputs)
      }
      case (a: TypeDef) :: as => {
        c.warning(c.enclosingPosition,
          "virtualization of type parameters is not supported.")
        (None, inputs)
      }
      case a :: as => (Some(a), as)
      case Nil     => (None, Nil)
    }

    /* Virtualize the annottee. */
    val expandees = annottee match {
      case Some(a) => transformer.virtualize(a)._1 :: rest
      case None    => rest
    }

    c.Expr(Block(expandees, Literal(Constant(()))))
  }

  private final class Virtualizer[C <: Context](val c: C)
    extends LanguageVirtualization {
    type Ctx = C
    val debugLevel = 0
  }
}


trait MacroModule {
  type Ctx <: Context
  val c: Ctx
}

trait DataDefs extends MacroModule {
  import c.universe._
  case class DSLFeature(tpe: Option[Type], name: String, targs: List[Tree], args: List[List[Type]])
}

/**
 * Common utilities for the Yin-Yang project.
 */
trait TransformationUtils extends MacroModule {
  import c.universe._
  import internal.decorators._

  /* These two should be unified */
  def method(recOpt: Option[Tree], methName: String, args: List[List[Tree]], targs: List[Tree] = Nil): Tree = {
    val calleeName = TermName(methName)
    val callee = recOpt match {
      case Some(rec) => Select(rec, calleeName)
      case None      => Ident(calleeName)
    }
    val calleeAndTargs: Tree = typeApply(targs)(callee)
    args.foldLeft(calleeAndTargs) { Apply(_, _) }
  }

  def symbolId(symbol: Symbol): Int =
    symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].id

  def symbolId(tree: Tree): Int = symbolId(tree.symbol)

  def typeApply(targs: List[Tree])(select: Tree) = if (targs.nonEmpty)
    TypeApply(select, targs)
  else
    select

  def makeConstructor(classname: String, arguments: List[Tree]): Tree =
    Apply(Select(newClass(classname), termNames.CONSTRUCTOR), arguments)

  def newClass(classname: String) =
    New(Ident(TypeName(classname)))

  def copy(orig: Tree)(nev: Tree): Tree = {
    nev.setSymbol(orig.symbol)
    nev.setPos(orig.pos)
    nev
  }

  def log(s: => String, level: Int = 0) = if (debugLevel > level) println(s)

  def debugLevel: Int

  /*
   * Utility methods for logging.
   */
  def className: String = ???
  lazy val typeRegex = new Regex("(" + className.replace("$", "\\$") + """\.this\.)(\w*)""")
  lazy val typetagRegex = new Regex("""(scala\.reflect\.runtime\.[a-zA-Z`]*\.universe\.typeTag\[)(\w*)\]""")
  def code(tree: Tree, shortenDSLNames: Boolean = true): String = {
    var short = showCode(tree)
    if (shortenDSLNames) {
      typeRegex findAllIn short foreach { m =>
        val typeRegex(start, typ) = m
        short = short.replace(start + typ, typ.toUpperCase())
      }
      typetagRegex findAllIn short foreach { m =>
        val typetagRegex(start, typ) = m
        short = short.replace(start + typ + "]", "TYPETAG[" + typ.toUpperCase() + "]")
      }
    }
    short
  }
}


trait LanguageVirtualization extends MacroModule with TransformationUtils with DataDefs {
  import c.universe._

  def virtualize(t: Tree): (Tree, Seq[DSLFeature]) = VirtualizationTransformer(t)

  object VirtualizationTransformer {
    def apply(tree: Tree) = {
      val t = new VirtualizationTransformer().apply(tree)
      log("(virtualized, Seq[Features]): " + t, 2)
      t
    }
  }

  private class VirtualizationTransformer extends Transformer {
    val lifted = mutable.ArrayBuffer[DSLFeature]()

    def liftFeature(receiver: Option[Tree], nme: String, args: List[Tree], targs: List[Tree] = Nil): Tree = {
      lifted += DSLFeature(receiver.map(_.tpe), nme, targs, List(args.map(_.tpe)))
      log(show(method(receiver.map(transform), nme, List(args.map(transform)), targs)), 3)
      method(receiver.map(transform), nme, List(args.map(transform)), targs)
    }

    override def transform(tree: Tree): Tree = atPos(tree.pos) {
      tree match {
        // sstucki: It seems necessary to keep the MUTABLE flag in the
        // new ValDef set, otherwise it becomes tricky to
        // "un-virtualize" a variable definition, if necessary
        // (e.g. if the DSL does not handle variable definitions in a
        // special way).
        case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.MUTABLE) =>
          ValDef(mods, sym, tpt, liftFeature(None, "__newVar", List(rhs)))

        // TODO: what about variable reads?

        case t @ If(cond, thenBr, elseBr) =>
          liftFeature(None, "__ifThenElse", List(cond, thenBr, elseBr))

        case Return(e) =>
          liftFeature(None, "__return", List(e))

        case Assign(lhs, rhs) =>
          liftFeature(None, "__assign", List(lhs, rhs))

        case LabelDef(sym, List(), If(cond, Block(body :: Nil, Apply(Ident(label),
          List())), Literal(Constant(())))) if label == sym => // while(){}
          liftFeature(None, "__whileDo", List(cond, body))

        case LabelDef(sym, List(), Block(body :: Nil, If(cond, Apply(Ident(label),
          List()), Literal(Constant(()))))) if label == sym => // do while(){}
          liftFeature(None, "__doWhile", List(cond, body))

        // only rewrite + to infix_+ if lhs is a String *literal* (we can't look at types!)
        // TODO: note that this is not sufficient to handle "foo: " + x + "," + y
        case Apply(Select(qual @ Literal(Constant(s: String)), TermName("$plus")), List(arg)) =>
          liftFeature(None, "infix_$plus", List(qual, arg))

//        case Apply(Select(qualifier, TermName("$plus")), List(arg)) =>
//          liftFeature(None, "infix_$plus", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("$eq$eq")), List(arg)) =>
          liftFeature(None, "infix_$eq$eq", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("$bang$eq")), List(arg)) =>
          liftFeature(None, "infix_$bang$eq", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("$hash$hash")), List()) =>
          liftFeature(None, "infix_$hash$hash", List(qualifier))

        case Apply(lhs @ Select(qualifier, TermName("equals")), List(arg)) =>
          liftFeature(None, "infix_equals", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("hashCode")), List()) =>
          liftFeature(None, "infix_hashCode", List(qualifier))

        case TypeApply(Select(qualifier, TermName("asInstanceOf")), targs) =>
          liftFeature(None, "infix_asInstanceOf", List(qualifier), targs)

        case TypeApply(Select(qualifier, TermName("isInstanceOf")), targs) =>
          liftFeature(None, "infix_isInstanceOf", List(qualifier), targs)

        case Apply(lhs @ Select(qualifier, TermName("toString")), List()) =>
          liftFeature(None, "infix_toString", List(qualifier))

        case Apply(lhs @ Select(qualifier, TermName("eq")), List(arg)) =>
          liftFeature(None, "infix_eq", List(qualifier, arg))

        case Apply(lhs @ Select(qualifier, TermName("ne")), List(arg)) =>
          liftFeature(None, "infix_ne", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("notify")), List()) =>
          liftFeature(None, "infix_notify", List(qualifier))

        case Apply(Select(qualifier, TermName("notifyAll")), List()) =>
          liftFeature(None, "infix_notifyAll", List(qualifier))

        case Apply(Select(qualifier, TermName("synchronized")), List(arg)) =>
          liftFeature(None, "infix_synchronized", List(qualifier, arg))

        case Apply(TypeApply(Select(qualifier, TermName("synchronized")), targs), List(arg)) =>
          liftFeature(None, "infix_synchronized", List(qualifier, arg), targs)

        case Apply(Select(qualifier, TermName("wait")), List()) =>
          liftFeature(None, "infix_wait", List(qualifier))

        case Apply(Select(qualifier, TermName("wait")), List(arg)
          ) if arg.tpe =:= typeOf[Long] =>
          liftFeature(None, "infix_wait", List(qualifier, arg))

        case Apply(Select(qualifier, TermName("wait")), List(arg0, arg1)
          ) if arg0.tpe =:= typeOf[Long] && arg1.tpe =:= typeOf[Int] =>
          liftFeature(None, "infix_wait", List(qualifier, arg0, arg1))

        case Try(block, catches, finalizer) => {
          c.warning(tree.pos, "virtualization of try/catch expressions is not supported.")
          super.transform(tree)
        }

        case Throw(expr) => {
          c.warning(tree.pos, "virtualization of throw expressions is not supported.")
          super.transform(tree)
        }

        case ClassDef(mods, n, _, _) if mods.hasFlag(Flag.CASE) =>
          // sstucki: there are issues with the ordering of
          // virtualization and expansion of case classes (i.e. some
          // of the expanded code might be virtualized even though it
          // should not be and vice-versa).  So until we have decided
          // how proper virtualization of case classes should be done,
          // any attempt to do so should fail.
          // TR: not 100% sure what the issue is (although i vaguely
          // remember that we had issues in Scala-Virtualized with 
          // auto-generated case class equality methods using virtualized
          // equality where it shouldn't). For the moment it seems like 
          // just treating case classes as regular classes works fine.
          //println(tree)
          c.warning(tree.pos, "virtualization of case classes is not fully supported.")
          super.transform(tree) 
        case _ =>
          super.transform(tree)
      }
    }
    def apply(tree: c.universe.Tree): (Tree, Seq[DSLFeature]) =
      (transform(tree), lifted.toSeq)
  }

}
