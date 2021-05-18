package org.virtuslab.stacktraces.parser

import scala.util.matching.Regex

/** 
 * Parser inspired on Java parser by @alexscheitlin
 * https://github.com/alexscheitlin/java-stack-trace-parser/blob/master/src/main/java/ch/scheitlin/alex/java/StackTraceParser.java  
 * 
 * A typical stack trace element looks like follows:
 * com.myPackage.myClass.myMethod(myClass.java:1)
 * component        example             allowed signs
 * ---------------- ------------------- ------------------------------------------------------------
 * package name:    com.myPackage       alphabetical / numbers
 * class name:      myClass             alphabetical / numbers / $-sign for anonymous inner classes
 * method name:     myMethod            alphabetical / numbers / $-sign for lambda expressions
 * file name:       myClass.java        alphabetical / numbers
 * line number:     1                   integer
 *
 * The following lines show some example stack trace elements:
 * org.junit.Assert.fail(Assert.java:86)                                            // typical stack trace element
 * sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)                      // native method
 * org.junit.runners.ParentRunner$1.schedule(ParentRunner.java:71)                  // anonymous inner classes
 * org.junit.runners.ParentRunner.access$000(ParentRunner.java:58)                  // lambda expressions
 * org.apache.maven.surefire.junit4.JUnit4TestSet.execute(JUnit4TestSet.java:53)    // numbers for package and class names
 *
 * Using the predefined structure of a stack trace element and allowed signs for its components, the following
 * regular expression can be used to parse stack trace elements and it's components. Parentheses ('(', ')') are used
 * to extract the components and '?:' is used to group the signs but not creating capture groups. Additionally, the
 * typical stack trace output has a leading tab and 'at ' before the stack trace element.
 *
 * ^(?: {4}|\t)at ((?:(?:[\d\w]*\.)*[\d\w]*))\.([\d\w\$]*)\.([\d\w\$]*)\((?:(?:([\d\w]*\.(?:scala|java)):(\d*))|([\d\w\s]*))\)$
 * group 1: package name
 * group 2: class name
 * group 3: method name
 * group 4: file name | null
 * group 5: line number | null
 * group 6: null | string
 * 
 */
object StacktraceParser:

  val stlRegex = "^(?: {4}|\\t)at ((?:(?:[\\d\\w]*\\.)*[\\d\\w]*))\\.([\\d\\w\\$]*)\\.([\\d\\w\\$]*)\\((?:(?:([\\d\\w]*\\.(?:scala|java)):(\\d*))|([\\d\\w\\s]*))\\)$".r

  def parse(stacktraceRaw: String): Either[String, Exception] =
    val stackTraceLines = stacktraceRaw.split("\n").toList
    val (head, stackTrace) = stackTraceLines.span(stlRegex.unapplySeq(_).isEmpty)
    val (errors, stackTraceElements) = stackTrace.partitionMap { line =>
      line match
        case stlRegex(packageName, className, methodName, fileName, lineNumberOrNull, nativeMethodOrNull) =>
          val lineNumber = Option(lineNumberOrNull).map(_.toInt).getOrElse {
            if nativeMethodOrNull == "Native Method" then -2 else -1
          }
          val ste = StackTraceElement(
            packageName + "." + className,
            methodName,
            fileName,
            lineNumber
          )
          if s"    at ${ste.toString}" != line then
            val msg = "ERROR: Stack trace line could not be parsed to StackTraceElement:\n" +
              s"Original stack trace line: $line\n" +
              s"Parsed StackTraceElement:     at ${ste.toString}"
            Left(msg)
          else
            Right(ste)
        case _ =>
          val msg = "ERROR: Couldn't match stacktrace to regex\n" +
            s"Originial stack trace line: $line\n"
          Left(msg)
    }
    if errors.nonEmpty then
      Left(s"Encountered errors while parsing: ${errors.mkString("\n")}")
    else
      val ex = Exception(head.mkString("\n"))
      ex.setStackTrace(stackTraceElements.toArray)
      Right(ex)
