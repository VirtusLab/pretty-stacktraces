package org.virtuslab.stacktraces
package tests

import org.virtuslab.stacktraces.core.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter


trait A:
  def doSthA = doSthAInlined

  @inline def doSthAInlined = throw IllegalStateException("doSthAInlined")

  def doSth = doSthInlined

  inline def doSthInlined = throw IllegalStateException("doSthInlined")

class B extends A


extension (n: Int)
  def !(n2: Int): Int =
    if math.random < n/10.0 then throw RuntimeException("error")
    n + n2

object BasicTests:

  @main 
  def nestedLambdas = TestExecutor.executeTest { () =>
      val y = 1
      val x = (0 to 10).flatMap { 
        n => List(n).map { 
          n => (if n > 5 then List(true) else List(false)).flatMap {
            n => (if n then List("0") else List("5")).map { n => n.toInt ! n.toInt ! n.toInt }
          }
        } 
      }
      val z = 1
    } 

  @main 
  def BdoSth = 
    TestExecutor.executeTest(() => B().doSth)

  @main 
  def BdoSthA = 
    TestExecutor.executeTest(() => B().doSthA)
