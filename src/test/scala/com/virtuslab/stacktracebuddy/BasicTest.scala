package com.virtuslab.stacktracebuddy

import com.virtuslab.stacktracebuddy.core.StackTraceBuddy
import com.virtuslab.stacktracebuddy.printer.PrettyExceptionPrinter

import org.junit.Test
import org.junit.Assert._

import java.net.URLClassLoader

extension (n: Int)
  def !(n2: Int): Int =
    if math.random < n/10.0 then throw RuntimeException("error")
    n + n2

class BasicTest:

  @Test 
  def stacktraceTest(): Unit = 
    try
      val x = (0 to 10).flatMap { 
        n => List(n).map { 
          n => (if n > 5 then List(true) else List(false)).flatMap {
            n => (if n then List("0") else List("5")).map {
              n => n.toInt ! n.toInt ! n.toInt
            }
          }
        } 
      }
    catch
      case e: Exception => 
        val prettyStackTrace = StackTraceBuddy.convertToPrettyStackTrace(e)
        PrettyExceptionPrinter.printStacktrace(prettyStackTrace)
        // e.getStackTrace.foreach(println)
