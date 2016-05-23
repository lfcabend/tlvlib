name := "tlvlib"

organization := "org.tlv"

version := "1.1-SNAPSHOT"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies := Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4")

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
