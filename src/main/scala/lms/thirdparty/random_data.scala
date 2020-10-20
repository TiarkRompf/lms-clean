package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

object RandomDataTypeLess {
  import BaseTypeLess._
  import PrimitiveTypeLess._
  import CLibTypeLess._

  // FIXME(feiw) add support for this:
  /* Intializes random number generator */
  // srand((unsigned) time(&t));

  def rand_int(implicit __pos: SourceContext): INT = INT(CMACRO("rand()", manifest[Int]))
  def rand_max(implicit __pos: SourceContext): INT = INT(CMACRO("RAND_MAX", manifest[Int]))

  def rand_float(implicit __pos: SourceContext): FLOAT = {
    val p = CAST_HELPER((rand_int - rand_max / 2), manifest[Int], manifest[Float], __pos)
    val q = CAST_HELPER(rand_max, manifest[Int], manifest[Float], __pos)
    FLOAT(NUM(p) / NUM(q))
  }

  def random_value(m: Manifest[_])(implicit __pos: SourceContext): TOP = m match {
    case ma if ma == manifest[Int] => rand_int
    case ma if ma == manifest[Float] => rand_float
    case ma => throw new Exception(s"manifest $ma is not yet handled in random_value function")
  }
}

trait RandomDataOps extends Base {

  import RandomDataTypeLess._

  def randInt(implicit __pos: SourceContext): Rep[Int] = Wrap[Int](rand_int.x)
  def randMax(implicit __pos: SourceContext): Rep[Int] = Wrap[Int](rand_max.x)
  def randFloat(implicit __pos: SourceContext): Rep[Float] = Wrap[Float](rand_float.x)
  def randomValue[T:Numeric:Manifest](implicit __pos: SourceContext): Rep[T] = Wrap[T](random_value(manifest[T]).x)
}
