organization := "trygvis"

name := "Artifact Repository"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.1"

scalacOptions += "-unchecked"

libraryDependencies += "net.databinder" %% "unfiltered-filter" % "0.5.1"

libraryDependencies += "net.databinder" %% "unfiltered-jetty" % "0.5.1"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.2.0"

libraryDependencies += "org.specs2" %% "specs2" % "1.6.1"
