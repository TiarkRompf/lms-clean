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

  def rand_int(implicit __pos: SourceContext): INT = INT(CMACRO("rand()", manifest[Int]))
  def rand_max(implicit __pos: SourceContext): INT = INT(CMACRO("RAND_MAX", manifest[Int]))

  def rand_float(implicit __pos: SourceContext): FLOAT = {
    val p = CAST_HELPER((rand_int - rand_max / 2), manifest[Int], manifest[Float], __pos)
    val q = CAST_HELPER(rand_max, manifest[Int], manifest[Float], __pos)
    FLOAT(NUM(p) / NUM(q))
  }
}

trait RandomDataOps extends Base {

  import RandomDataTypeLess._

  def randInt(implicit __pos: SourceContext): Rep[Int] = Wrap[Int](rand_int.x)
  def randMax(implicit __pos: SourceContext): Rep[Int] = Wrap[Int](rand_max.x)
  def randFloat(implicit __pos: SourceContext): Rep[Float] = Wrap[Float](rand_float.x)
}