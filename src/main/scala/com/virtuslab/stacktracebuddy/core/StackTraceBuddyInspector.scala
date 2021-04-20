package com.virtuslab.stacktracebuddy.core

import com.virtuslab.stacktracebuddy.model.PrettyException
import com.virtuslab.stacktracebuddy.model.PrettyStackTraceElement
import com.virtuslab.stacktracebuddy.model.ElementType
import com.virtuslab.stacktracebuddy.io.ClasspathDirectoriesLoader
import com.virtuslab.stacktracebuddy.io.TastyFilesLocator

import dotty.tools.dotc.util.NameTransformer
import dotty.tools.dotc.core.Names

import scala.quoted.*
import scala.tasty.inspector.*
import scala.collection.JavaConverters.*

import java.io.File


object StackTraceBuddyInspector:
  def inspectStackTrace(ste: StackTraceElement, tastyFile: File): PrettyStackTraceElement =
    val stackTraceBuddyInspector = StackTraceBuddyInspector(ste)
    TastyInspector.inspectTastyFiles(List(tastyFile.toString))(stackTraceBuddyInspector)
    stackTraceBuddyInspector.prettyStackTrace


class StackTraceBuddyInspector private (ste: StackTraceElement) extends Inspector:
  private var prettyStackTrace: PrettyStackTraceElement = null
  override def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import quotes.reflect.*

    for tasty <- tastys do
      val tree = tasty.ast
      val defdefs = walkInOrder(tree)
      processDefDefs(defdefs)
    
    def walkInOrder(tree: Tree): List[DefDef] =
      if tree.pos.startLine < ste.getLineNumber then
        visitTree(tree)
      else 
        Nil

    def visitTree(tree: Tree): List[DefDef] =
      tree match
        case PackageClause(_, list) => 
          list.flatMap(walkInOrder)
        case ClassDef(_, _, _, _, list) => 
          list.flatMap(walkInOrder)
        case d @ DefDef(name, _, _, rhs) => 
          val defdef = if d.pos.startLine + 1 <= ste.getLineNumber && d.pos.endLine + 1 >= ste.getLineNumber then
            List(d)
          else
            Nil
          defdef ++ walkInOrder(rhs.get)
        case ValDef(_, _, rhs) => 
          walkInOrder(rhs.get)
        case Try(body, _, _) => 
          walkInOrder(body)
        case Block(list, term) => 
          list.flatMap(walkInOrder) ++ walkInOrder(term)
        case Apply(term, list) => 
          walkInOrder(term) ++ list.flatMap(walkInOrder)
        case TypeApply(term, _) => 
          walkInOrder(term)
        case Select(term, _) => 
          walkInOrder(term)
        case _ =>
          Nil
    
    def processDefDefs(defdefs: List[DefDef]): Unit =
      val decoded = NameTransformer.decode(Names.termName(ste.getMethodName))
      prettyStackTrace = decoded.toString match
        case d if d.contains("$anonfun$") =>
          val lambdas = defdefs.filter(_.name == "$anonfun")
          createPrettyStackTraceElement(lambdas.head) // TODO: FIX
        case d =>
          val fun = defdefs.filter(_.name == d)
          fun match
            case head :: Nil => 
              createPrettyStackTraceElement(head)
            case _ =>
              throw IllegalStateException(s"There should be only one function matching, found ${fun.size}")

    def createPrettyStackTraceElement(d: DefDef): PrettyStackTraceElement =
      PrettyStackTraceElement(ste, label(d), d.symbol.fullName, d.pos.sourceFile.jpath.toString)

    def label(d: DefDef): ElementType =  d.symbol match
      case s if s.flags.is(Flags.ExtensionMethod) => ElementType.ExtensionMethod
      case s if s.name == "$anonfun" => ElementType.Lambda("(Int) => Int", s.owner.name) // TODO: FIX
      case _ => ElementType.Method
