package org.virtuslab.stacktraces.core.mycore

import org.virtuslab.stacktraces.core.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter

// Must not be in default package
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, State, Scope}


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

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Thread)
class Benchmarks:

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def method: Unit =
    try
      val x = (0 to 10).flatMap { 
        n => (0 to n).map {  
          n => B() ! n.toInt
        }
      }
    catch
      case e: Exception =>
        val prettyStackTrace = Stacktraces.convertToPrettyStackTrace(e)
        // PrettyExceptionPrinter.printStacktrace(prettyStackTrace)
