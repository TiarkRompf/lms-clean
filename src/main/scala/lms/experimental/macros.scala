package lms.experimental

import language.experimental.macros
import scala.annotation.StaticAnnotation

class ir extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ir_impl.impl
}
