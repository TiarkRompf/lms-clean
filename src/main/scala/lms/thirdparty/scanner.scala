package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.collection.mutable.ArrayOps

import lms.collection._

// Examples of using lib-function for simple file scanning
trait ScannerOps extends Equal with ArrayOps with RangeOps with CLibs {

  // FIXME(feiw): should the open and close function have CTRL effects??
  // open function returns a file discriptor
  def open(path: Rep[String]): Rep[Int] = libFunction[Int]("open", Unwrap(path), lms.core.Backend.Const(0))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)
  // filelen function returns the file size
  def filelen(fd: Rep[Int]): Rep[Long] = libFunction[Long]("fsize", Unwrap(fd))(Seq[Int](), Seq[Int](), Set[Int]())
  // static_cast function cast types
  def static_cast[X,Y:Manifest](x: Rep[X]): Rep[Y] =
    Wrap[Y](Adapter.g.reflect("cast", Unwrap(x), Backend.Const(manifest[Y])))
  // need a macro for mmap
  def prot = cmacro[Int]("PROT_READ | PROT_WRITE, MAP_FILE | MAP_PRIVATE")
  // mmap function maps a file to memory
  def mmap[T:Manifest](fd: Rep[Int], len: Rep[Long]) = static_cast[Array[T], Array[T]](libFunction[Array[T]]("mmap",
    lms.core.Backend.Const(0), Unwrap(len), Unwrap(prot), Unwrap(fd), lms.core.Backend.Const(0))(Seq[Int](), Seq[Int](), Set[Int]()))
  def close(fd: Rep[Int]) = libFunction[Unit]("close", Unwrap(fd))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)
  def prints(s: Rep[String]): Rep[Int] = libFunction[Int]("printll", Unwrap(s))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  // establish a File* type
  abstract class FilePointer
  // another API for opening a file
  def fopen(name: Rep[String], mode: Rep[String]) = libFunction[FilePointer]("fopen", Unwrap(name), Unwrap(mode))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)
  def fclose(fp: Rep[FilePointer]) = libFunction[Unit]("fclose", Unwrap(fp))(Seq[Int](0), Seq[Int](), Set[Int](), Adapter.CTRL)
  def checkStatus(code: Rep[Int])(implicit pos: SourceContext): Rep[Unit] = __ifThenElse(notequals(code, unit(1)),
    libFunction[Unit]("perror", lms.core.Backend.Const("Error reading file"))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL), ())

  def getFloat(fp: Rep[FilePointer], target: Var[Float])(implicit pos: SourceContext) =
    checkStatus(libFunction[Int]("fscanf", Unwrap(fp), lms.core.Backend.Const("%f"), UnwrapV(target))(Seq[Int](0), Seq[Int](2), Set[Int](2)))
  def getFloat(fp: Rep[FilePointer], target: Rep[Array[Float]], target_offset: Rep[Int])(implicit pos: SourceContext) =
    checkStatus(libFunction[Int]("fscanf", Unwrap(fp), lms.core.Backend.Const("%f"), Unwrap(target(target_offset)))(Seq[Int](0), Seq[Int](), Set[Int](2), Unwrap(target)))
  def getInt(fp: Rep[FilePointer], target: Var[Int])(implicit pos: SourceContext) =
    checkStatus(libFunction[Int]("fscanf", Unwrap(fp), lms.core.Backend.Const("%d"), UnwrapV(target))(Seq[Int](0), Seq[Int](2), Set[Int](2)))
  def getInt(fp: Rep[FilePointer], target: Rep[Array[Int]], target_offset: Rep[Int])(implicit pos: SourceContext) =
    checkStatus(libFunction[Int]("fscanf", Unwrap(fp), lms.core.Backend.Const("%d"), Unwrap(target(target_offset)))(Seq[Int](0), Seq[Int](), Set[Int](2), Unwrap(target)))
  def getElement[T:Manifest](fp: Rep[FilePointer], target: Rep[Array[T]], target_offset: Rep[Int])(implicit pos: SourceContext) = {
    val format = manifest[T] match {
      case m if m == manifest[Float] => "%f"
      case m if m == manifest[Int] => "%d"
      case m => throw new Exception(s"not yet supporting manifest ${m}")
    }
    checkStatus(libFunction[Int]("fscanf", Unwrap(fp), lms.core.Backend.Const(format), Unwrap(target(target_offset)))(Seq[Int](0), Seq[Int](), Set[Int](2), Unwrap(target)))
  }

  def readFileToArray[T:Manifest](name:Rep[String], mode:Rep[String], array: Rep[Array[T]], size:Rep[Int])(implicit pos: SourceContext) = {
    val fp = fopen(name, mode)
    for (i <- (0 until size): Rep[Range])
      getElement[T](fp, array, i)
    fclose(fp)
  }

  // writing to file in C
  def fprintf(fp: Rep[FilePointer], format: Rep[String], contents: Rep[Any]*) = {
    val inputs = Seq(Unwrap(fp), Unwrap(format)) ++ contents.map(Unwrap)
    libFunction[Unit]("fprintf", inputs: _*)((Range(0, inputs.length): Range).toSeq, Seq(0), Set[Int]())
  }

  // some helper functions that directly use C code implementation in scanner_header.h
  def scanFile[T:Manifest](name: Rep[String], array: Rep[Array[T]], size: Rep[Int]) = manifest[T] match {
    case m if m == manifest[Float] =>
      libFunction[Unit]("scan_floats", Unwrap(name), Unwrap(array), Unwrap(size))(Seq(0), Seq(1), Set[Int]())
    case m if m == manifest[Int] =>
      libFunction[Unit]("scan_ints", Unwrap(name), Unwrap(array), Unwrap(size))(Seq(0), Seq(1), Set[Int]())
    case m => throw new Exception(s"not yet supporting manifest ${m} in scanFile function")
  }

  def checkFile[T:Manifest](name: Rep[String], array: Rep[Array[T]], size: Rep[Int]) = manifest[T] match {
    case m if m == manifest[Float] =>
      libFunction[Unit]("check_float_array", Unwrap(name), Unwrap(array), Unwrap(size))(Seq(0, 1), Seq[Int](), Set[Int](), Adapter.CTRL)
    case m if m == manifest[Int] =>
      libFunction[Unit]("check_int_array", Unwrap(name), Unwrap(array), Unwrap(size))(Seq(0, 1), Seq[Int](), Set[Int](), Adapter.CTRL)
    case m => throw new Exception(s"not yet supporting manifest ${m} in checkFile function")
  }
}

trait CCodeGenScannerOps extends ExtendedCCodeGen with CCodeGenLibs {
  // need to register the headers
  registerHeader("\"scanner_header.h\"")
  registerDefine("_GNU_SOURCE")

  // type remap
  override def remap(m: Manifest[_]) = m.runtimeClass.getName match {
    case s: String if s.endsWith("FilePointer") => "FILE*"
    case _ => super.remap(m)
  }
}
