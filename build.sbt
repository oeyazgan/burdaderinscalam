ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.someorganization"
ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "playground"
  )

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.17"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"

logBuffered in Test := false