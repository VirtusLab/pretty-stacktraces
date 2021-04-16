package com.virtuslab.stacktracebuddy

object PrettyStackTracePrinter:
  def print(pst: List[PrettyStackTrace]): Unit =
    pst.foreach { p =>
      if p != null then
        println(p.original)
        println(p.element)
      else
        p
    }
