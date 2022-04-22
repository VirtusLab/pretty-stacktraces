package org.virtuslab.stacktraces

import scala.io.Source

import org.virtuslab.stacktraces.core.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter

import org.junit.Assert.assertTrue
import org.junit.Assume.assumeTrue
import org.junit.Test

import java.nio.file.Paths

trait TestExecutor:

  val test: () => Unit

  @Test
  final def executeTest =
    // Enable test only on JDK 11
    assumeTrue(System.getProperty("java.version").startsWith("11."))

    val className = this.getClass.getName.split("\\.")
    val expected = Source.fromResource(Paths.get(className.head, className.tail*).toString).mkString

    try
      test()
    catch
      case e: Throwable =>
        val prettyStackTrace = convertToPrettyStackTraceWithStdlib(e)
        assertTrue(PrettyExceptionPrinter.prettyStacktrace(prettyStackTrace).build.replaceAll("\u001b\\[[;\\d]*m", "").contains(expected))

  private def convertToPrettyStackTraceWithStdlib(e: Throwable) =
    Stacktraces.convertToPrettyStackTrace(e, Seq("scala-library_3-3.1.3-RC2-bin-SNAPSHOT.jar"))
