package org.virtuslab.stacktraces.printer

import org.virtuslab.stacktraces.model.ElementType
import org.virtuslab.stacktraces.model.PrettyStackTraceElement
import org.virtuslab.stacktraces.model.PrettyException


object PrettyExceptionPrinter:

  def printStacktrace(pe: PrettyException): Unit =
    val pst = prettyStackTrace {
      addWithColor(LIGHT_ORANGE)(s"Exception in thread ${Thread.currentThread.getName}: ")
      addWithColor(RED)(s"${pe.original.getClass.getName}: ${pe.original.getMessage}")
      add("\n")
      val errors = pe.prettyStackTrace.flatMap(_.error).distinct
      pe.prettyStackTrace.foreach {
        case PrettyStackTraceElement(ste, elementType, prettyName, prettyFile, lineNumber, opJarName, error) =>
          add("    at ")
          elementType match
            case ElementType.Lambda(tpe, parent) =>
              add("lambda ")
              addWithColor(AMBER)(tpe)
              add(s" of ${parent} ")
            case _ =>
              add(s"${elementType.name} ")
              val clr = if error.isDefined then RED else AMBER
              addWithColor(clr)(s"$prettyName ")
          val lineNumberOrNativeMethod = if ste.isNativeMethod then "(Native method)" else lineNumber
          add("in ")
          addWithColor(GREEN)(prettyFile)
          add(":")
          addWithColor(BLUE)(s"$lineNumberOrNativeMethod ")
          opJarName match
            case Some(name) =>
              add("inside ")
              addWithColor(LIGHT_PURPLE)(s"$name ")
            case None =>
              // do nothing
          error match
            case Some(er) =>
              addWithColor(RED)(s"[${errors.indexOf(er) + 1}]")
            case None =>
              // do nothing
          add("\n")
      }
      errors.zipWithIndex.foreach { (er, id) =>
        addWithColor(RED)(s"[${id + 1}]")
        add(s" - ${er.msg}")
      }
    }
    println(pst.build)
