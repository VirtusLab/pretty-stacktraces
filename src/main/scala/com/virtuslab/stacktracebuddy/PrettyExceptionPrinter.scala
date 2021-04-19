package com.virtuslab.stacktracebuddy

import Console.{GREEN, RED, RESET, YELLOW_B, UNDERLINED}


object PrettyExceptionPrinter:
  def print(pe: PrettyException): Unit =
    Console.println(s"${RESET}${RED}Exception in thread ${Thread.currentThread().getName()}: ${pe.original.getClass.getName}: ${pe.original.getMessage}${RESET}")


    println(pe.original.printStackTrace)
    // pe.prettyStackTrace.foreach { p =>
    //   if p != null then
    //     println(p.original)
    //     println(p.element)
    //   else
    //     p
    // }
