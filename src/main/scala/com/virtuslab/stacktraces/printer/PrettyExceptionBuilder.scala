package org.virtuslab.stacktraces.printer

import Console.RESET

val RED = "\u001b[38;5;203m"
val LIGHT_ORANGE = "\u001b[38;5;215m"
val BLUE = "\u001b[38;5;39m"
val GREEN = "\u001b[38;5;120m"
val AMBER = "\u001b[38;5;142m"
val LIGHT_PURPLE = "\u001b[38;5;141m"
val GRAY = "\u001b[38;5;248m"

class PrettyStackTrace:
    val stackTraceBuilder = new StringBuilder
    def build = stackTraceBuilder.toString
    def ++=(s: String): Unit = stackTraceBuilder ++= s

def prettyStackTrace(init: PrettyStackTrace ?=> Unit): PrettyStackTrace =
  given p: PrettyStackTrace = PrettyStackTrace()
  init
  p

def changeColor(color: String)(using p: PrettyStackTrace) =
  p ++= color

def reset(using p: PrettyStackTrace) =
  p ++= RESET

def add(str: String)(using p: PrettyStackTrace) =
  p ++= str

def addWithColor(color: String)(str: String)(using p: PrettyStackTrace) =
  p ++= s"$color$str$RESET"

