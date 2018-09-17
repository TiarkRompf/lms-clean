name := "lms-clean"

organization := "org.scala-lang.virtualized"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.4"

val paradiseVersion = "2.1.0"

//crossScalaVersions := Seq("2.12.1")

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

libraryDependencies += ("org.scala-lang" % "scala-reflect" % "2.12.4")

libraryDependencies += ("org.scala-lang" % "scala-compiler" % "2.12.4" % "compile")

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

// --- testing ---

// tests are not thread safe
parallelExecution in Test := false
