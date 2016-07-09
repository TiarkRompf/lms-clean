package scala.lms

import language.experimental.{macros => m}
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

import scala.util.matching.Regex

import scala.collection.mutable


class ir extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro macros.ir.impl
}

class virtualize extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro macros.virtualize.impl
}

