fork := true

name := "tlvlib"

organization := "org.tlv"

version := "1.2-SNAPSHOT"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies +=  "com.lihaoyi" %% "fastparse" % "1.0.0"

libraryDependencies +=  "com.lihaoyi" %% "fastparse-byte" % "1.0.0"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
