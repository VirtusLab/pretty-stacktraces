package org.virtuslab.stacktraces.tests

import org.virtuslab.stacktraces.core.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter

trait A:
  def doSth(n: Int) = doSthNested(n)
  def doSthNested(n: Int) = doSthInlined(n)
  extension[T] (t: T) def let[R](l: (T) => R): R = l(t)
  inline def doSthInlined(n: Int): Int =
    if math.random < n/10.0 then n.let { 
      n => n.let { n => throw RuntimeException("error") }
    }
    n

class B extends A

extension (b: B)
  def !(n: Int): Int = b.doSth(n)

def main(args: Array[String]) =
  try
    val x = (0 to 10).flatMap { 
      n => (0 to n).map {  
        n => B() ! n.toInt
      }
    }
  catch
    case e: Exception =>
      if args.isEmpty then
        val prettyStackTrace = Stacktraces.convertToPrettyStackTrace(e)
        PrettyExceptionPrinter.printStacktrace(prettyStackTrace)
      else
        e.printStackTrace
