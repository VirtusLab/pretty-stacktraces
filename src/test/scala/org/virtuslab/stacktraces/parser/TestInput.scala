package org.virtuslab.stacktraces.parser

import org.junit.Test

extension (n: Int)
  def !(n2: Int): Int =
    if math.random < n/10.0 then throw RuntimeException("TestInput")
    n + n2

class TestInput:

  //@Test
  def test =
    val y = 1
    val x = (0 to 10).flatMap { 
      n => List(n).map { 
        n => (if n > 5 then List(true) else List(false)).flatMap {
          n => (if n then List("0") else List("5")).map { n => n.toInt ! n.toInt ! n.toInt }
        }
      } 
    }
    val z = 1
