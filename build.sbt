name := "lms-clean"

organization := "org.scala-lang.virtualized"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.10"

val paradiseVersion = "2.1.0"

//crossScalaVersions := Seq("2.12.1")

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

libraryDependencies += ("org.scala-lang" % "scala-reflect" % scalaVersion.value)

libraryDependencies += ("org.scala-lang" % "scala-compiler" % scalaVersion.value % "compile")

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.12.0" % "1.0.3")

libraryDependencies += "org.scala-lang.plugins" % "scala-continuations-library_2.12" % "1.0.3"

scalacOptions += "-P:continuations:enable"

// --- testing ---

// tests are not thread safe
parallelExecution in Test := false
