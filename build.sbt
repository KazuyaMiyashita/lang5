import Dependencies._

lazy val root = project
  .in(file("."))
  .settings(
    name := "lang5",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.2.1",
    libraryDependencies += scalaTest % Test
  )
