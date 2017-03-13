fork := true

name := "tlvlib"

organization := "org.tlv"

version := "1.2-SNAPSHOT"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies +=  "com.lihaoyi" %% "fastparse" % "0.4.2"

libraryDependencies +=  "com.lihaoyi" %% "fastparse-byte" % "0.4.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
