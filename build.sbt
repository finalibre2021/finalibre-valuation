val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "finalibre-valuation",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.typesafe" % "config" % "1.4.1"
  )
