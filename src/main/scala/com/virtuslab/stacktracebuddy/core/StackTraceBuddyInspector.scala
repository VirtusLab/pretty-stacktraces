package com.virtuslab.stacktracebuddy.core

import com.virtuslab.stacktracebuddy.model.PrettyException
import com.virtuslab.stacktracebuddy.model.PrettyStackTraceElement
import com.virtuslab.stacktracebuddy.model.ElementType
import com.virtuslab.stacktracebuddy.io.ClasspathDirectoriesLoader
import com.virtuslab.stacktracebuddy.io.TastyFilesLocator
import com.virtuslab.stacktracebuddy.tasty.TypesSupport

import dotty.tools.dotc.util.NameTransformer
import dotty.tools.dotc.core.Names

import scala.quoted.*
import scala.tasty.inspector.*
import scala.collection.JavaConverters.*

import java.io.File


object StackTraceBuddyInspector:
  def inspectStackTrace(ste: StackTraceElement, tastyFile: File): Option[PrettyStackTraceElement] =
    val stackTraceBuddyInspector = StackTraceBuddyInspector(ste)
    TastyInspector.inspectTastyFiles(List(tastyFile.toString))(stackTraceBuddyInspector)
    stackTraceBuddyInspector.prettyStackTrace


class StackTraceBuddyInspector private (ste: StackTraceElement) extends Inspector:
  private var prettyStackTrace: Option[PrettyStackTraceElement] = None
  override def inspect(using q: Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import q.reflect.*
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
        case Import(_, _) => 
          Nil
        case Export(_, _) =>
          Nil
        case ClassDef(_, _, _, _, list) => 
          list.flatMap(walkInOrder)
        case TypeDef(_, rhs) =>
          walkInOrder(rhs)
        case d @ DefDef(name, _, _, rhs) => 
          val defdef = if d.pos.startLine + 1 <= ste.getLineNumber && d.pos.endLine + 1 >= ste.getLineNumber then
            List(d)
          else
            Nil
          defdef ++ walkInOrder(rhs.get)
        case ValDef(_, _, rhs) => 
          walkInOrder(rhs.get)
        case Ident(_) =>
          Nil
        case Select(term, _) => 
          walkInOrder(term)
        case Literal(_) =>
          Nil
        case This(_) =>
          Nil
        case New(_) =>
          Nil
        case NamedArg(_, _) =>
          Nil
        case Apply(term, list) => 
          walkInOrder(term) ++ list.flatMap(walkInOrder)
        case TypeApply(term, _) => 
          walkInOrder(term)
        case Super(_, _) =>
          Nil
        case Typed(_, _) =>
          Nil
        case Assign(lhs, rhs) =>
          walkInOrder(lhs) ++ walkInOrder(rhs)
        case Block(list, term) => 
          list.flatMap(walkInOrder) ++ walkInOrder(term)        
        case Closure(term, _) =>
          walkInOrder(term)
        case If(a, b, c) =>
          walkInOrder(a) ++ walkInOrder(b) ++ walkInOrder(c)
        case Match(term, _) =>
          walkInOrder(term)
        case SummonFrom(_) =>
          Nil       
        case Try(tr, _, fin) => 
          walkInOrder(tr) ++ fin.fold(Nil)(walkInOrder(_))
        case Return(term, _) =>
          walkInOrder(term)
        case Repeated(list, _) => 
          list.flatMap(walkInOrder)
        case Inlined(_, _, body) =>
          walkInOrder(body)
        case SelectOuter(term, _, _) =>
          walkInOrder(term)
        case While(cond, body) =>
          walkInOrder(cond) ++ walkInOrder(body)
        case _: TypeTree =>
          Nil
        case x =>
          println(s"Unmatched param: $x")
          Nil
    
    def processDefDefs(defdefs: List[DefDef]): Unit =
      val decoded = NameTransformer.decode(Names.termName(ste.getMethodName)).toString
      prettyStackTrace = decoded match
        case d if d.contains("$anonfun$") =>
          val lambdas = defdefs.filter(f => f.name == "$anonfun" && f.pos.endLine + 1 == ste.getLineNumber)
          lambdas match
            case head :: Nil =>
              val ts = TypesSupport(q)
              createPrettyStackTraceElement(head, head.pos.startLine + 1)
            case _ =>
              val excMsg = """
              | Too many nested lambdas in one line, cannot disambiguate. 
              | For debugging purposes try to make every nested lambda to be separate line, e. g.
              | list.map { x => x.map { y => ... } }
              | would be
              | list.map { x => x.map {
              |   y => ...
              | } }
              """.trim
              throw IllegalStateException(excMsg)
        case d =>
          defdefs match
            case Nil =>
              None
            case head :: Nil =>
              createPrettyStackTraceElement(head, ste.getLineNumber)
            case _ => 
              val fun = defdefs.filter(_.name == d)
              fun match // This will probably fail for nested inline functions, though we cannot disambiguate them
                case head :: Nil =>
                  createPrettyStackTraceElement(head, ste.getLineNumber)
                case defdefs =>
                  val excMsg = s"""
                  | Couldnt disambiguate function for this stack frame $ste, possible reasons:
                  | - Nested inline function inside another function (due to name mangling they are hard to locate)
                  """.trim
                  throw IllegalStateException(excMsg)

    def createPrettyStackTraceElement(d: DefDef, lineNumber: Int): Some[PrettyStackTraceElement] =
      Some(PrettyStackTraceElement(ste, label(d), d.name, d.pos.sourceFile.jpath.toString, lineNumber))

    def label(d: DefDef): ElementType =  d.symbol match
      case s if s.flags.is(Flags.ExtensionMethod) => ElementType.ExtensionMethod
      case s if s.name == "$anonfun" => 
        val ts = TypesSupport(q)
        val ownerName = s.owner.name
        val parent = if ownerName == "$anonfun" then "some outer lambda" else ownerName
        ElementType.Lambda(ts.toLambda(d.asInstanceOf[ts.qctx.reflect.DefDef]), parent)
      case _ => ElementType.Method
