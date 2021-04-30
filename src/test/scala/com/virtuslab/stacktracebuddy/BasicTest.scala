package com.virtuslab.stacktracebuddy

import com.virtuslab.stacktracebuddy.core.StackTraceBuddy
import com.virtuslab.stacktracebuddy.printer.PrettyExceptionPrinter

import org.junit.Test
import org.junit.Assert._

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

class BasicTest:

  private def executeTest(test: () => Unit) =
    try
      test()
    catch
      case e: Exception =>
        val prettyStackTrace = StackTraceBuddy.convertToPrettyStackTrace(e)
        PrettyExceptionPrinter.printStacktrace(prettyStackTrace)

  @Test 
  def nestedLambdas = executeTest { () =>
      val y = 1
      val x = (0 to 10).flatMap { 
        n => List(n).map { 
          n => (if n > 5 then List(true) else List(false)).flatMap {
            n => (if n then List("0") else List("5")).map { 
              n => n.toInt ! n.toInt ! n.toInt 
            }
          }
        } 
      }
      val z = 1
    } 

  @Test 
  def BdoSth = 
    executeTest(() => B().doSth)

  @Test 
  def BdoSthA = 
    executeTest(() => B().doSthA)
