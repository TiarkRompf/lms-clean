name := "lms-clean"

organization := "org.scala-lang.virtualized"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.2"

scalaOrganization := "org.scala-lang.virtualized" // for now, keep using virtualized

val paradiseVersion = "2.0.1"

crossScalaVersions := Seq("2.11.2")

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.2" % "test"
)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "compile")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "compile")

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

