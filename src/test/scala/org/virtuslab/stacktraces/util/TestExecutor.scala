package org.virtuslab.stacktraces

import scala.io.Source

import org.virtuslab.stacktraces.core.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter

import org.junit.Assert.assertTrue
import org.junit.Test

import java.nio.file.Paths

trait TestExecutor:

  val test: () => Unit

  @Test
  final def executeTest =

    val className = this.getClass.getName.split("\\.")
    val expected = Source.fromResource(Paths.get(className.head, className.tail*).toString).mkString

    try
      test()
    catch
      case e: Exception =>
        val prettyStackTrace = convertToPrettyStackTraceWithStdlib(e)
        assertTrue(PrettyExceptionPrinter.prettyStacktrace(prettyStackTrace).build.replaceAll("\u001b\\[[;\\d]*m", "").endsWith(expected))

  private def convertToPrettyStackTraceWithStdlib(e: Exception) =
    Stacktraces.convertToPrettyStackTrace(e, Seq("scala-library_3-3.1.0-RC1-bin-SNAPSHOT.jar"))
