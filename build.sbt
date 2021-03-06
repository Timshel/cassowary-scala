sbtPlugin := true

name := "cassowary-scala"

organization := "epfl"

version := "0.0.1"

scalaVersion  := "2.10.3"

description := "sbt plugin to create a single fat jar"

scalacOptions := Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq( "org.slf4j" % "slf4j-api" % "1.7.5" )