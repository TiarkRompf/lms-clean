package lms
package core

import language.experimental.{macros => m}
import scala.annotation.StaticAnnotation


class ir extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro macros.ir.impl
}

class virtualize extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro macros.virtualize.impl
}
