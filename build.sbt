val scala3Version = "3.0.0-RC2"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "com.virtuslab",
    name := "stacktracebuddy",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
    excludeDependencies += "org.scala-lang.scala-library",
    excludeDependencies += "org.scala-lang" % "scala3-library-bootstrapped"
  )
