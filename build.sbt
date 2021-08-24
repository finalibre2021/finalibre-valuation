val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "finalibre-valuation",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= List(
      "com.typesafe" % "config" % "1.4.1",
      "org.apache.commons" % "commons-math3" % "3.6.1",
      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
      "org.jfree" % "jfreechart" % "1.5.3" % "test"
    )
  )
