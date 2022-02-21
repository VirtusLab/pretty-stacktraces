package org.virtuslab.stacktraces
package tests
package stackoverflow

import org.virtuslab.stacktraces.core.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter

class Factorial extends TestExecutor:
  override val test = () =>
    def factorial(n : Int): Int =
      if n==1 then
        1
      else
        n * factorial(n-1)
    factorial(500000)
