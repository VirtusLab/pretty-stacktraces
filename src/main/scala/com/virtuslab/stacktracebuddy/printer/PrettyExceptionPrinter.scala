package com.virtuslab.stacktracebuddy.printer

import com.virtuslab.stacktracebuddy.model.ElementType
import com.virtuslab.stacktracebuddy.model.PrettyStackTraceElement
import com.virtuslab.stacktracebuddy.model.PrettyException

import Console.{RESET, UNDERLINED}


val RED = "\u001b[38;5;196m"
val LIGHT_RED = "\u001b[38;5;198m"
val GREEN = "\u001b[38;5;46m"
val AMBER = "\u001b[38;5;142m"

object PrettyExceptionPrinter:
  def printStacktrace(pe: PrettyException): Unit =
    Console.println(s"${RESET}${LIGHT_RED}Exception in thread ${Thread.currentThread().getName()}: ${RESET}${RED}${pe.original.getClass.getName}: ${pe.original.getMessage}${RESET}")
    pe.prettyStackTrace.foreach { 
      case PrettyStackTraceElement(ste, elementType, prettyName, prettyFile, lineNumber, opJarName) =>
        print("    at ")
        elementType match
          case ElementType.Lambda(tpe, parent) =>
            print(s"lambda ${AMBER}${tpe}${RESET} of ${parent} ")
          case _ =>
            print(s"${elementType.name} ${AMBER}${prettyName}${RESET} ")
        val lineNumberOrNativeMethod = if ste.isNativeMethod then "(Native method)" else lineNumber
        print(s"in ${GREEN}${prettyFile}${RESET}:${RED}${lineNumberOrNativeMethod}${RESET} ")
        opJarName match
          case Some(name) =>
            print(s"inside ${name}")
          case None =>

        print("\n")
    }
