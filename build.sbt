name := "argument parser"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.25"

scalacOptions += "-feature"
scalacOptions += "-deprecation"

initialCommands in console := "import scalaz._, Scalaz._"