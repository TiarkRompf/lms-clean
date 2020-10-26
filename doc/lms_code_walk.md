# LMS Code Walk

## Abstract

Lightweight Modular Staging (LMS) is a generative programming tool achieving the programming paradigm called multi-stage programming (staging).
LMS_clean is a remake of the LMS project, aiming at a more flexible design and extension with better support for
LMS IR transformation and optimization. This documentation is a code-walk of the LMS_clean repo, hoping to
explain the implementation (in high-level or in details) to people who are interested in learning and using LMS.
This is different from a tutorial since it will dive into the core implementation of LMS and offers more insights
than simply how to use LMS.

## Introduction

Multi-Stage Programming (or staging) is a programming language concept that allows various parts of the programs
to run in different stages. It allows users to code with high-level abstractions (trait, classes, higher-order functions),
but still gain highly-efficient code after the abstractions are executed (staged away). This offers both high
productivity and high performance of the target program, thus the slogan "abstract without regret".

Lightweight Modular Staging (LMS) is a staging tool built in Scala. In LMS, type information is used to distinguish
the evaluation stages (i.e., All Rep[T] typed values and expressions are in code generation for the next stage).
Simply speaking, LMS is a compiler. However, LMS does not have a laxer or parser to transform the input program
into intermediate representations (IR). Instead, the IR is generated via executing the input program. All the Rep[T]
typed expressions in the input program evaluate to LMS IR. This can be considered as the LMS frontend.
Then the LMS backend compiles the LMS IR to target programs.

## Overview

The basic components of LMS support includes:
1. LMS IR: defining how the core LMS IR looks like.
2. CodeGen: defining how to generate code in the target language.
3. FrontEnd: defining how to construct the LMS IR nicely.

The basic LMS IR is defined in this file `lms-clean/src/main/scala/lms/core/backend.scala`:
[LMS Core](main/scala/lms/core/backend.md)

The simple typeless frontend is defined in this file `lms-clean/src/main/scala/lms/core/frontend.scala`:
[LMS Simple Frontend](main/scala/lms/core/frontend.md)

The iconic typed (`Rep[T]`) frontend is defined in this file `lms-clean/src/main/scala/lms/core/stub.scala`:
[LMS Frontend](main/scala/lms/core/stub.md)

The basic codegen support is in the file `lms-clean/src/main/scala/lms/core/codegen.scala`
and the folder `lms-clean/src/main/scala/lms/core/codegen/*`.
TODO(feiw): add a markdown file for codegen.


### Extension

Besides the basic LMS support mentioned above, LMS has included a number of extensions:
1. Collections (Array, List, Tuple, ...)
2. ThirdParty library (MPI, NCCL, ...)
3. Other Types (fp16, tensor, ...)

The extensions normally include implementations of:
1. Extension of FrontEnd (traits of construction of new ops).
2. Extension of CodeGen (how new ops are generated to the target language).
3. Extension of Graph Transformation (how new ops are transformed).

### Collections

Several examples of collections are implemented here (`lms-clean/src/main/scala/lms/collection`). Taking the ListOps.scala as example, it has four traits in total.

The `trait ListOps` is the frontend trait of `Rep[List]`. It contains an `object List` for the construction of `Rep[List]`-typed values, and an `implicit class ListOps` for the methods that are available for `Rep[List]`-typed values.

The `trait ListOpsOpt` is the optimized frontend trait of `Rep[List]`, where several
construction-time optimization are introduced.

The `trait ScalaCodeGen_List` is the trait for generating scala code from the nodes
created by `trait ListOps` and `trait ListOpsOpt`. The most important function in this
trait is `override def shallow` to generate code for each `Rep[List]` node.

The `trait CppCodeGen_List` is the trait for generating C++ code from the nodes
created by `trait ListOps` and `trait ListOpsOpt`. Similarly, the most important
function in this trait is `override def shallow` that generates code for each `Rep[List]` node.

### ThirdParty Library

Several examples of third party libraries are implemented here (`lms-clean/src/main/scala/lms/thirdparty`). The key functionality is provide by `lms-clean/src/main/scala/lms/thirdparty/c_lib_utils.scala`.

In the `c_lib_utils.scala` file, we provide the following functionalities:
1. Create a macro by the `cmacro` function.
2. Create a C struct by `newStruct` function.
3. Create a call to library functions by `libFunction` function.

We only create three types of nodes for thirdparty libraries.
1. The `cmacro` function creates nodes with `op = "cmacro"`.
2. The `newStruct` function creates nodes with `op = "lib-struct"`.
3. The `libFunction` function creates nodes with `op = "lib-Function"`.

The codegen supporting those nodes are in this same file.
To mix in traits for the frontend support, just extend the `trait CLibs`.
To mix in traits for the codegen support, just extend the `trait CCodeGenLibs`.

## Put Everything Together: Driver/Compiler

With the support in frontends and codegen, we want to introduce a driver or a compiler that puts all the supports together and offers the programming environment for user application, code generation, compilation, and evaluation.

The basic components of a driver or compiler include:
1. All the needed frontend traits.
2. All the needed codegen traits.
3. An abstract method `snippet`, which will be overridden by the user application code
4. Facilities to print code to files, compile them, and evaluate them.

For example:

```scala
abstract class DslDriverC[A: Manifest, B: Manifest] extends DslSnippet[A, B] with DslExp { self =>
  val codegen = new DslGenC {
    val IR: self.type = self
  }
  lazy val (code, statics) = {
    val source = new java.io.ByteArrayOutputStream()
    val statics = codegen.emitSource[A,B](wrapper, "Snippet", new java.io.PrintStream(source))
    (source.toString, statics)
  }
  val compilerCommand = "cc -std=c99 -O3"
  def libraries = codegen.libraryFlags mkString(" ")

  val sourceFile = "/tmp/snippet.c"
  val executable = "/tmp/snippet"
  lazy val f: A => Unit = {
    // TBD: should read result of type B?
    val out = new java.io.PrintStream(sourceFile)
    out.println(code)
    out.close
    (new java.io.File(executable)).delete
    import scala.sys.process._
    val includes = codegen.joinPaths(codegen.includePaths, "-I")
    val libraryPaths = codegen.joinPaths(codegen.libraryPaths, "-L")
    val pb: ProcessBuilder = s"$compilerCommand $sourceFile -o $executable $libraries $includes $libraryPaths"
    time("gcc") { pb.lines.foreach(Console.println _) }
    (a: A) => (s"$executable $a": ProcessBuilder).lines.foreach(Console.println _)
  }
  def eval(a: A): Unit = { val f1 = f; time("eval")(f1(a)) }
}
```

The `DslSnippet[A, B]` is the trait that defines the abstract `def snippet` function.
The `DslExp` is all the frontend support.
The `val codegen = new DslGenC{ val IR: self.type = self }` is all the codegen support.
The `lazy val (code, statics)` is the code emission support.
The `lazy val f` is the compilation support.
The `def eval` is the evaluation support.

To build a driver for your specific domain specific language, you might need to extend more frontend traits
and more codegen traits. For example:

```scala
abstract class DslDriverCMPI[A:Manifest, B:Manifest] extends DslDriverC[A,B] with MPIOps { q =>
    override val codegen = new DslGenC with CCodeGenMPI with CCodeGenLibs {
      val IR: q.type = q
    }
}
```
More frontend traits are mixed in after `extends DslDriverC[A,B]`.
More codegen traits are mixed in after `new DslGenC`.
You can also override the compilation supports.

There might be cases where you want to introduce graph transformations before code generation.
If so, please use and extend this `trait CompilerC`:

```scala
abstract class CompilerC[A:Manifest, B:Manifest] extends DslDriverC[A, B] { q =>

  // get original graph
  val graph = Adapter.genGraph1(manifest[A], manifest[B])(x => Unwrap(wrapper(Wrap[A](x))))

  // run some transformation
  def transform(graph: Graph): Graph = graph

  // codegen
  override lazy val (code, statics) = {
    val source = new java.io.ByteArrayOutputStream()
    val statics = time("codegen") {
      val final_g = transform(graph)
      codegen.typeMap = Adapter.typeMap
      codegen.stream = new java.io.PrintStream(source)
      codegen.emitAll(final_g, "Snippet")(manifest[A], manifest[B])
      codegen.extractAllStatics.toList
    }
    (source.toString, statics)
  }
}
```

where you can override the `def transform` to add in your desired transformation.
