package com.virtuslab.stacktracebuddy


case class Pos(line: Int, column: Int)
case class PrettyStackTrace(original: StackTraceElement, element: String)
