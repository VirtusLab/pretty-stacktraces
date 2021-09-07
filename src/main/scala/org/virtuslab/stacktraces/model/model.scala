package org.virtuslab.stacktraces.model

import java.io.File

case class ClasspathWrapper(file: File, jarName: Option[String])
case class TastyWrapper(file: File, jar: Option[File])
case class PrettyException(original: Throwable, prettyStackTrace: List[PrettyStackTraceElement])

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
  jarName: Option[String] = None,
  error: Option[PrettyErrors] = None,
  isTasty: Boolean = true
)

enum PrettyErrors(val msg: String):
  case InlinedLambda extends PrettyErrors(
    """Too many nested lambdas in one line, cannot disambiguate. 
    | For debugging purposes try to make every nested lambda to be in a separate line, e. g.
    | list.map { x => x.map { y => ... } }
    | would be
    | list.map { x => x.map {
    |   y => ...
    | } }
    |""".stripMargin
  )
  case Unknown extends PrettyErrors(
    """Couldn't disambiguate function, possible reasons:
    | - Nested inline function inside another function (due to name mangling they are hard to locate)
    |""".stripMargin
  )
