package com.virtuslab.stacktracebuddy.model

enum ElementType(val name: String):
  case Method extends ElementType("method")
  case ExtensionMethod extends ElementType("extension method")
  case Lambda(val tpe: String, parent: String) extends ElementType("labmda")

case class PrettyStackTraceElement(original: StackTraceElement, elementType: ElementType, prettyName: String, prettyFile: String)
