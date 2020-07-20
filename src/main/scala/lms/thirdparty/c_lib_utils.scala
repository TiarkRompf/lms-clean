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


trait CMacro { b: Base =>
  def cmacro[T:Manifest](m:String): Rep[T] = {
    Wrap[T](Adapter.g.reflectUnsafe("macro", lms.core.Backend.Const(m)))
  }
}
trait CCodeGenCMacro extends ExtendedCCodeGen {
  override def shallow(n: Node): Unit = n match {
    case Node(_, "macro", List(Const(m:String)), _) =>
      emit(m)
    case _ => super.shallow(n)
  }
}

trait LibStruct { b : Base =>
  def newStruct[T: Manifest]: Rep[T] = {
    Wrap[T](Adapter.g.reflectUnsafe("lib-struct", Backend.Const(manifest[T])))
  }

  // T: type of struct . U: type of field
  def readField[T: Manifest, U: Manifest](x: Rep[T], m: String): Rep[U] = {
    Wrap[U](Adapter.g.reflectRead("read-field", Unwrap(x), Backend.Const(m))(Unwrap(x)))
  }
}

trait CCodeGenLibStruct extends ExtendedCCodeGen {
  override def mayInline(n: Node): Boolean = n match {
    case Node(_, "lib-struct", _, _) => false
    case _ => super.mayInline(n)
  }

  override def shallow(n: Node): Unit = n match {
    case Node(s, "read-field", List(x: Sym, Const(m: String)), _) =>
      shallow(x); emit(s".$m")
    case _ => super.shallow(n)
  }

  override def traverse(n: Node): Unit = n match {
    case Node(s, "lib-struct", List(Const(m: Manifest[_])), _) =>
      emit(s"${remap(m)} ")
      shallow(s)
      emitln(";")
    // allow "lib-struct" to take parameters
    case Node(s, "lib-struct", Const(m: Manifest[_])::args, _) =>
      emit(s"${remap(m)} "); shallow(s);
      emit("("); shallow(args.head);
      args.tail.foreach(a => {emit(", "); shallow(a)})
      emitln(");")
    case _ => super.traverse(n)
  }
}

trait LibFunction { b: Base =>
  def libFunction[T:Manifest](m:String, rhs:lms.core.Backend.Exp*)(rkeys:Seq[Int], wkeys:Seq[Int], pkeys:Set[Int], keys: lms.core.Backend.Exp*): Rep[T] = {
    val readKeys = rkeys.map(rhs(_))
    val writeKeys = wkeys.map(rhs(_)) ++ keys
    val defs = Seq(lms.core.Backend.Const(m), lms.core.Backend.Const(pkeys)) ++ rhs
    Wrap[T](Adapter.g.reflectEffect("lib-function", defs:_*)(readKeys: _*)(writeKeys: _*))
  }
}

trait CCodeGenLibFunction extends ExtendedCCodeGen {
  override def shallow(n: Node): Unit = n match {
    case Node(s, "lib-function", Const(m:String) :: Const(pkeys: Set[Int]) :: rhs, _) =>
      val last = rhs.length - 1
      emit(s"$m(");
      rhs.zipWithIndex.foreach { case(r, index) =>
        if (pkeys.contains(index)) emit("&")
        shallow(r)
        if (index < last) emit(", ")
      }
      emit(")")
    case _ => super.shallow(n)
  }
}

trait CLibs extends Base with CMacro with LibStruct with LibFunction
trait CCodeGenLibs extends CCodeGenCMacro with CCodeGenLibStruct with CCodeGenLibFunction

trait CudaFunction extends Base {
  def cudaFunction[T:Manifest](m: String, configs: Seq[lms.core.Backend.Exp],
    rhs: lms.core.Backend.Exp*)(rkeys: Seq[Int], wkeys: Seq[Int], pkeys: Set[Int], keys: lms.core.Backend.Const*): Rep[T] = {
    val readKeys = rkeys.map(rhs(_))
    val writeKeys = wkeys.map(rhs(_)) ++ keys
    val defs = Seq(lms.core.Backend.Const(m), lms.core.Backend.Const(configs.length), lms.core.Backend.Const(pkeys)) ++ configs ++ rhs
    Wrap[T](Adapter.g.reflectEffect("lib-function", defs: _*)(readKeys: _*)(writeKeys: _*))
  }
}

trait CudaCodeGenLibFunction extends ExtendedCCodeGen {
  override def shallow(n: Node): Unit = n match {
    case Node(s, "lib-function", Const(m:String)::Const(c:Int)::Const(pkeys:Set[Int])::defs, _) =>
      val configs = defs.take(c)
      val rhs = defs.drop(c)
      val last = rhs.length - 1
      emit(s"$m<<<"); shallow(configs.head);
      configs.tail.foreach(a => {emit(", "); shallow(a)});
      emit(">>>(");
      rhs.zipWithIndex.foreach { case(r, index) =>
        if (pkeys.contains(index)) emit("&")
        shallow(r)
        if (index < last) emit(", ")
      }
      emit(")")
    case _ => super.shallow(n)
  }
}


// Examples of using lib-function for simple file scanning
trait ScannerOps extends Equal with ArrayOps with CLibs {

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

  def getFloat(fp: Rep[FilePointer], target: Var[Float]) =
    checkStatus(libFunction[Int]("fscanf", Unwrap(fp), lms.core.Backend.Const("%f"), UnwrapV(target))(Seq[Int](0), Seq[Int](2), Set[Int](2)))
  def getFloat(fp: Rep[FilePointer], target: Rep[Array[Float]], target_offset: Rep[Int]) =
    checkStatus(libFunction[Int]("fscanf", Unwrap(fp), lms.core.Backend.Const("%f"), Unwrap(target(target_offset)))(Seq[Int](0), Seq[Int](), Set[Int](2), Unwrap(target)))
  def getInt(fp: Rep[FilePointer], target: Var[Int]) =
    checkStatus(libFunction[Int]("fscanf", Unwrap(fp), lms.core.Backend.Const("%d"), UnwrapV(target))(Seq[Int](0), Seq[Int](2), Set[Int](2)))
  def getInt(fp: Rep[FilePointer], target: Rep[Array[Int]], target_offset: Rep[Int]) =
    checkStatus(libFunction[Int]("fscanf", Unwrap(fp), lms.core.Backend.Const("%d"), Unwrap(target(target_offset)))(Seq[Int](0), Seq[Int](), Set[Int](2), Unwrap(target)))

  // writing to file in C
  def fprintf(fp: Rep[FilePointer], format: Rep[String], contents: Rep[Any]*) = {
    val inputs = Seq(Unwrap(fp), Unwrap(format)) ++ contents.map(Unwrap)
    libFunction[Unit]("fprintf", inputs: _*)((Range(0, inputs.length): Range).toSeq, Seq(0), Set[Int]())
  }
}

trait CCodeGenScannerOps extends ExtendedCCodeGen with CCodeGenLibs {

  // need to register the headers
  val workPath = sys.props("LMS_PATH")
  val headerPath = "src/main/scala/lms/thirdparty/thirdpartyAdaptor/"
  val headerFile = "<scanner_header.h>"
  registerHeader(s"$workPath/$headerPath", headerFile)
  registerDefine("_GNU_SOURCE")

  // type remap
  override def remap(m: Manifest[_]) = m.runtimeClass.getName match {
    case s: String if s.endsWith("FilePointer") => "FILE*"
    case _ => super.remap(m)
  }
}
