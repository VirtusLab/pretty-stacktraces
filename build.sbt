enablePlugins(JmhPlugin)

inThisBuild(List(
  sonatypeProfileName := "org.virtuslab",
  organization := "org.virtuslab",
  homepage := Some(url("https://github.com/VirtusLab/pretty-stacktraces")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "BarkingBad",
      "Andrzej Ratajczak",
      "andrzej.ratajczak98@gmail.com",
      url("https://twitter.com/aj_ratajczak")
    ),
    Developer(
      "romanowski",
      "Krzysztof Romanowski",
      "kromanowski@virtuslab.com",
      url("https://twitter.com/RomanowskiKr")
    )
  )
))

ThisBuild / parallelExecution := false

Global / excludeLintKeys += ThisBuild / organization

val scala3Version = "3.1.3-RC2"

lazy val root = project
  .in(file("."))
  .enablePlugins(JmhPlugin)
  .settings(
    organization := "org.virtuslab",
    name := "pretty-stacktraces",

    scalaVersion := scala3Version,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
  )
