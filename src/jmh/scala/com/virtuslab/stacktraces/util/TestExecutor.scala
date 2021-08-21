package org.virtuslab.stacktraces

import org.virtuslab.stacktraces.core.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter


object TestExecutor:
  def executeTest(test: () => Unit) =
    try
      test()
    catch
      case e: Exception =>
        val prettyStackTrace = convertToPrettyStackTraceWithStdlib(e)
        PrettyExceptionPrinter.printStacktrace(prettyStackTrace)

def convertToPrettyStackTraceWithStdlib(e: Exception) =
  Stacktraces.convertToPrettyStackTrace(e, Seq("scala-library_3-3.1.0-RC1-bin-SNAPSHOT.jar"))
