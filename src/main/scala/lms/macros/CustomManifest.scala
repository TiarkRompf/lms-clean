package lms.macros

trait CustomManifest[T] extends Manifest[T] {
  def runtimeClass = classOf[Int]
}