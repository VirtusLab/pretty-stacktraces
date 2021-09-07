package org.virtuslab.stacktraces.printer

import org.virtuslab.stacktraces.model.ElementType
import org.virtuslab.stacktraces.model.PrettyStackTraceElement
import org.virtuslab.stacktraces.model.PrettyException

import Console.RESET

object PrettyExceptionPrinter:

  def prettyStacktrace(pe: PrettyException, withJarName: Boolean = false): PrettyStackTrace =
    prettyStackTrace {
      addWithColor(LIGHT_ORANGE)(s"Exception in thread ${Thread.currentThread.getName}: ")
      addWithColor(RED)(s"${pe.original.getClass.getName}: ${pe.original.getMessage}")
      add("\n")
      val errors = pe.prettyStackTrace.flatMap(_.error).distinct
      pe.prettyStackTrace.foreach {
        case PrettyStackTraceElement(ste, elementType, prettyName, prettyFile, lineNumber, opJarName, error, isTasty) =>
          add("    at ")
          elementType match
            case ElementType.Lambda(tpe, parent) =>
              add("lambda ")
              addWithColor(AMBER)(tpe)
              add(s" of ${parent} ")
            case _ =>
              add(s"${elementType.name} ")
              val clr = error match
                case Some(_) => RED 
                case None => isTasty match
                  case true => AMBER
                  case false => GRAY
              addWithColor(clr)(s"$prettyName ")
          val lineNumberOrNativeMethod = if ste.isNativeMethod then "(Native method)" else lineNumber
          add("in ")
          addWithColor(GREEN)(prettyFile)
          add(":")
          addWithColor(BLUE)(s"$lineNumberOrNativeMethod ")
          opJarName match
            case Some(name) if withJarName =>
              add("inside ")
              addWithColor(LIGHT_PURPLE)(s"$name ")
            case _ =>
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

  def printStacktrace(pe: PrettyException, withJarName: Boolean = false): Unit =
    val pst = prettyStacktrace(pe, withJarName)
    println(pst.build)
