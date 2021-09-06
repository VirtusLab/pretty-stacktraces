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
  def nestedLambdasRec = TestExecutor.executeTest { () =>
      def rec(int: Int, string: String): IterableOnce[Any] =
        val y = 1
        val x = (0 to 10).flatMap { 
          n => 
            if int == 1 then
              throw new RuntimeException("abc")
            else if int > 0 then
              rec(int - 1, "ASD")
            else
              def rec(n: Int): IterableOnce[Any] = List(n).map { 
                n => (if n > 5 then List(true) else List(false)).flatMap {
                  n => (if n then List("0") else List("5")).map { n => 
                    22
                  }
                }
              } 
              rec(n)
        }
        x
      rec.apply(3, "ASD")
    } 

  @main
  def nestedLambdasRec2 = TestExecutor.executeTest { () =>
    def rec(int: Int) =
      throw new RuntimeException("abc")
    rec.apply(3)
  } 

  @main
  def nestedLambdasRec3 = TestExecutor.executeTest { () =>
    def rec(int: String) =
      List("asv").map { x =>
        val aaa = 1 + 1
        throw new RuntimeException("abc")
        2 + 2
      }
    List(1).map { n =>
      rec.apply("asd")
    }
  } 

  @main
  def nestedLambdasRec4 = TestExecutor.executeTest { () =>
    def rec(int: String) =
      throw new RuntimeException("abc")
    List(1).map { n =>
      rec.apply("asd")
    }
  } 

  @main
  def nestedLambdasRec5 = TestExecutor.executeTest { () =>
    def rec(int: String) =
      throw new RuntimeException("abc")
    List(1).map(n => {List(1).map(_ => 1.3); rec.apply("asd")})
  } 

  @main
  def nestedLambdasRec6 = TestExecutor.executeTest { () =>
    def rec(int: String) =
      throw new RuntimeException("abc")
    List(1).map(n => {List(1).map(_ => 1.3); List("xd").map(rec)})
  } 

  @main
  def nestedLambdasRec7 = TestExecutor.executeTest { () =>
    List(1).map { n =>
      List("xd").map { m =>
        throw new RuntimeException("xyz")
      }
    }
  } 

  @main 
  def BdoSth = 
    TestExecutor.executeTest(() => B().doSth)

  @main 
  def BdoSthA = 
    TestExecutor.executeTest(() => B().doSthA)
