package com.virtuslab.stacktracebuddy.model

import java.io.File

case class ClasspathWrapper(file: File, jarName: Option[String])
case class TastyWrapper(file: File, jarName: Option[String])
case class PrettyException(original: Exception, prettyStackTrace: List[PrettyStackTraceElement])

enum ElementType(val name: String):
  case Method extends ElementType("method")
  case ExtensionMethod extends ElementType("extension method")
  case Lambda(val tpe: String, parent: String) extends ElementType("labmda")

case class PrettyStackTraceElement(
  original: StackTraceElement,
  elementType: ElementType, 
  prettyName: String, 
  prettyFile: String, 
  lineNumber: Int,
  jarName: Option[String] = None
)
