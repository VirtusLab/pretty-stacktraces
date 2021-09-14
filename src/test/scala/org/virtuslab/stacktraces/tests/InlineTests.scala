package org.virtuslab.stacktraces
package tests
package inline

import org.virtuslab.stacktraces.core.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter

/* Inline test setup */

trait A:
  def doSthA = doSthAInlined
  @inline def doSthAInlined = throw IllegalStateException("doSthAInlined")
  def doSth = doSthInlined
  inline def doSthInlined = throw IllegalStateException("doSthInlined")


class B extends A

/* Inline test suites */

class BdoSth extends TestExecutor:
  override val test = () => B().doSth


class BdoSthA extends TestExecutor:
  override val test = () => B().doSthA
