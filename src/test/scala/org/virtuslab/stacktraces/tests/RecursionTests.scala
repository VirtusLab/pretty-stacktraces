package org.virtuslab.stacktraces
package tests
package recursion

import org.virtuslab.stacktraces.core.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter

class NestedLambdasRec extends TestExecutor:
  override val test = () =>
    def rec(int: Int, string: String): IterableOnce[Any] =
      val y = 1
      val x = (0 to 10).flatMap { 
        n => 
          if int == 1 then
            throw new RuntimeException("NestedLambdasRec")
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

    
class NestedLambdasRec2 extends TestExecutor:
  override val test = () =>
    def rec(int: Int) =
      throw new RuntimeException("NestedLambdasRec2")
    rec.apply(3)


class NestedLambdasRec3 extends TestExecutor:
  override val test = () =>
    def rec(int: String) =
      List("asv").map { x =>
        val aaa = 1 + 1
        throw new RuntimeException("NestedLambdasRec3")
        2 + 2
      }
    List(1).map { n =>
      rec.apply("asd")
    }


class NestedLambdasRec4 extends TestExecutor:
  override val test = () =>
    def rec(int: String) =
      throw new RuntimeException("NestedLambdasRec4")
    List(1).map { n =>
      rec.apply("asd")
    }


class NestedLambdasRec5 extends TestExecutor:
  override val test = () =>
    def rec(int: String) =
      throw new RuntimeException("NestedLambdasRec5")
    List(1).map(n => {List(1).map(_ => 1.3); rec.apply("asd")})


class NestedLambdasRec6 extends TestExecutor:
  override val test = () =>
    def rec(int: String) =
      throw new RuntimeException("NestedLambdasRec6")
    List(1).map(n => {List(1).map(_ => 1.3); List("xd").map(rec)})


class NestedLambdasRec7 extends TestExecutor:
  override val test = () =>
    List(1).map { n =>
      List("xd").map { m =>
        throw new RuntimeException("NestedLambdasRec7")
      }
    } 


class NestedLambdasRec8 extends TestExecutor: 
  override val test = () => 
    def rec(int: Int): Int =
      List(2).map { x =>
        if int == 0 then
          throw new RuntimeException("NestedLambdasRec8")
        else rec(int - 1)
      }
      1
    rec(3)

class ALotOfRecursiveCalls extends TestExecutor: 
  override val test = () => 
    def rec(i: Int): Int =
      if i <= 0 then
        rec1(23)
      else
        i + rec(i - 1)
    def rec1(i: Int): Int =
      if i <= 0 then
        rec2(5)
      else
        i + rec1(i - 1)
    def rec2(i: Int): Int =
      if i <= 0 then
        throw new RuntimeException("ALotOfRecursiveCalls")
      else
        i + rec2(i - 1)
    rec(120)

class ALotOfRecursiveCalls2 extends TestExecutor: 
  override val test = () => 
    def rec(i: Int): Int =
      if i <= 0 then
        rec3(23)
      else
        i + rec1(i - 1)
    def rec1(i: Int): Int =
      if i <= 0 then
        rec3(23)
      else
        i + rec2(i - 1)
    def rec2(i: Int): Int =
      if i <= 0 then
        rec3(23)
      else
        i + rec(i - 1)
    def rec3(i: Int): Int =
      if i <= 0 then
        rec4(5)
      else
        i + rec3(i - 1)
    def rec4(i: Int): Int =
      if i <= 0 then
        throw new RuntimeException("ALotOfRecursiveCalls2")
      else
        i + rec4(i - 1)
    rec(120)
