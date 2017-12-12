import sbt._

name := "PLDA-homework-02"

scalaVersion := "2.12.4"

scalacOptions += "-Ypartial-unification"

//You don't necessarily need them, you can uncomment this
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.0-RC1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
)
//dependencies used only for tests
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % Test
)
