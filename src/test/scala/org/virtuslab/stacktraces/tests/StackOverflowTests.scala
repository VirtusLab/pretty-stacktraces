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


class DualFactorial extends TestExecutor:
  override val test = () =>
    def factorial1(n : Int): Int =
      if n==1 then
        1
      else
        n * factorial2(n-1)
    def factorial2(n : Int): Int =
      if n==1 then
        1
      else
        n * factorial1(n-1)
    factorial1(500000)
