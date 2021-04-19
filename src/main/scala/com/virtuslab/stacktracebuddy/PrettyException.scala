package com.virtuslab.stacktracebuddy


case class Pos(line: Int, column: Int)
case class PrettyException(original: Exception, prettyStackTrace: List[PrettyStackTrace])
case class PrettyStackTrace(original: StackTraceElement, element: String)
