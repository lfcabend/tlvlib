name := "tlvlib"

organization := "org.tlv"

version := "1.2-SNAPSHOT"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies +=  "com.lihaoyi" %% "fastparse" % "0.4.1"

libraryDependencies +=  "com.lihaoyi" %% "fastparse-byte" % "0.4.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.8"

