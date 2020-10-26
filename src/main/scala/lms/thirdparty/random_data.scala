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

  /* Intializes random number generator */
  // Note that the `srand` must have a write effect to UNSAFE, so that it can be generated :)
  def unsigned_time0(implicit __pos: SourceContext): INT = INT(CMACRO("(unsigned)time(0)", manifest[Int]))
  def srand_from_time(time: INT)(implicit __pos: SourceContext): UNIT = UNIT(
    LIB_FUNCTION(manifest[Unit], "srand", unsigned_time0.x)(Seq[Int](), Seq[Int](), Set[Int](), UNSAFE)
  )

  def rand_int(implicit __pos: SourceContext): INT = INT(CMACRO("rand()", manifest[Int]))
  def rand_int(ub: Int)(implicit __pos: SourceContext): INT = INT(CMACRO(s"(rand() % $ub) + 1", manifest[Int]))
  def rand_max(implicit __pos: SourceContext): INT = INT(CMACRO("RAND_MAX", manifest[Int]))

  def rand_float(implicit __pos: SourceContext): FLOAT = {
    val p = (rand_int - rand_max / 2).castTo(manifest[Float])
    val q = rand_max.castTo(manifest[Float])
    FLOAT(NUM(p) / NUM(q))
  }

  def random_value(m: Manifest[_])(implicit __pos: SourceContext): TOP =
    if (m == manifest[Int]) rand_int
    else if (m == manifest[Float]) rand_float
    else throw new Exception(s"manifest $m is not yet handled in random_value function")
}

trait RandomDataOps extends Base {

  import RandomDataTypeLess._

  def srandFromTime(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](srand_from_time(unsigned_time0).x)
  def randInt(implicit __pos: SourceContext): Rep[Int] = Wrap[Int](rand_int.x)
  def randInt(ub: Int)(implicit __pos: SourceContext): Rep[Int] = Wrap[Int](rand_int(ub).x)
  def randMax(implicit __pos: SourceContext): Rep[Int] = Wrap[Int](rand_max.x)
  def randFloat(implicit __pos: SourceContext): Rep[Float] = Wrap[Float](rand_float.x)
  def randomValue[T:Numeric:Manifest](implicit __pos: SourceContext): Rep[T] = Wrap[T](random_value(manifest[T]).x)
}

trait CCodeGenRandomData extends ExtendedCCodeGen {
  registerHeader("<time.h>")
}
