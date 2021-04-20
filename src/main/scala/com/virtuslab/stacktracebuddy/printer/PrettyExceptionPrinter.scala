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
  def print(pe: PrettyException): Unit =
    Console.println(s"${RESET}${LIGHT_RED}Exception in thread ${Thread.currentThread().getName()}: ${RESET}${RED}${pe.original.getClass.getName}: ${pe.original.getMessage}${RESET}")
    pe.prettyStackTrace.foreach { 
      case PrettyStackTraceElement(ste, ElementType.Lambda(tpe, parent), prettyName, prettyFile) =>
        println(s"    at lambda ${AMBER}${tpe}${RESET} of ${parent} in ${GREEN}${prettyFile}${RESET}:${RED}${ste.getLineNumber}${RESET}")
      case PrettyStackTraceElement(ste, elementType, prettyName, prettyFile) =>
        println(s"    at ${elementType.name} ${AMBER}${prettyName}${RESET} in ${GREEN}${prettyFile}${RESET}:${RED}${ste.getLineNumber}${RESET}")
    }
