package com.virtuslab.stacktracebuddy

import org.junit.Test
import org.junit.Assert._

extension (n: Int)
  def !(n2: Int): Int =
    if math.random < n/10.0 then throw RuntimeException("error")
    n + n2




class BasicTest:
  @Test 
  def stacktraceTest(): Unit = 
    try
      (0 to 10).map { n => n ! n ! n }
    catch
      case e: Exception => e.getStackTrace.foreach(println)
