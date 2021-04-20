package com.virtuslab.stacktracebuddy.model

case class PrettyException(original: Exception, prettyStackTrace: List[PrettyStackTraceElement])
