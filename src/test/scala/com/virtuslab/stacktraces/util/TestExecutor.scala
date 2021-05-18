package org.virtuslab.stacktraces.util

import org.virtuslab.stacktraces.core.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter

import org.junit.Test
import org.junit.Assert._


object TestExecutor:
  def executeTest(test: () => Unit) =
    try
      test()
    catch
      case e: Exception =>
        val prettyStackTrace = Stacktraces.convertToPrettyStackTrace(e)
        PrettyExceptionPrinter.printStacktrace(prettyStackTrace)
