package org.virtuslab.stacktraces.core

import org.virtuslab.stacktraces.model.PrettyException
import org.virtuslab.stacktraces.model.PrettyStackTraceElement
import org.virtuslab.stacktraces.model.ElementType
import org.virtuslab.stacktraces.model.PrettyErrors
import org.virtuslab.stacktraces.io.TastyFilesLocator
import org.virtuslab.stacktraces.tasty.TypesSupport


import dotty.tools.dotc.util.NameTransformer
import dotty.tools.dotc.core.Names

import scala.quoted.*
import scala.tasty.inspector.*
import scala.collection.JavaConverters.*

import java.io.File
import scala.collection.mutable.ListBuffer
import org.virtuslab.stacktraces.model.TastyWrapper


object StacktracesInspector:
  def inspectStackTrace(st: List[StackTraceElement], tastyFiles: List[TastyWrapper], ctp: Map[String, String]): List[PrettyStackTraceElement] =
    val stacktracesInspector = StacktracesInspector(st, ctp)
    TastyInspector.inspectTastyFiles(tastyFiles.map(_.file.toString))(stacktracesInspector)
    stacktracesInspector.prettyStackTraceElements.toList


class StacktracesInspector private (st: List[StackTraceElement], ctp: Map[String, String]) extends Inspector:
  private val prettyStackTraceElements: ListBuffer[PrettyStackTraceElement] = ListBuffer.empty
  
  override def inspect(using q: Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import q.reflect.*

    val ts = TypesSupport(q)

    def label(d: DefDef): ElementType =  d.symbol match
      case s if s.flags.is(Flags.ExtensionMethod) => ElementType.ExtensionMethod
      case s if s.name == "$anonfun" => 
        
        val ownerName = s.owner.name
        val parent = if ownerName == "$anonfun" then "some outer lambda" else ownerName
        ElementType.Lambda(ts.toLambda(d.asInstanceOf[ts.qctx.reflect.DefDef]), parent)
      case _ => ElementType.Method
          
    def createPrettyStackTraceElement(d: DefDef, lineNumber: Int)(using ste: StackTraceElement): PrettyStackTraceElement =
      val nameWithoutPrefix = d.pos.sourceFile.jpath.toString.stripPrefix("out/bootstrap/stdlib-bootstrapped/scala-3.0.0-bin-SNAPSHOT-nonbootstrapped/src_managed/main/scala-library-src/") // TODO: Remove when stdlib will be shipped with tasty files!
      PrettyStackTraceElement(ste, label(d), d.name, nameWithoutPrefix, lineNumber)

    def createErrorWhileBrowsingTastyFiles(error: PrettyErrors)(using ste: StackTraceElement): PrettyStackTraceElement =
      PrettyStackTraceElement(ste, ElementType.Method, ste.getMethodName, ste.getClassName, ste.getLineNumber, error = Some(error))

    def walkInOrder(tree: Tree)(using ste: StackTraceElement): List[DefDef] =
      if tree.pos.startLine < ste.getLineNumber then
        visitTree(tree)
      else 
        Nil
    
    def visitTree(tree: Tree)(using ste: StackTraceElement): List[DefDef] =
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
          defdef ++ rhs.fold(Nil)(walkInOrder)
        case ValDef(_, _, rhs) => 
          rhs.fold(Nil)(walkInOrder)
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
        case Typed(term, _) =>
          walkInOrder(term)
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
    

    def processDefDefs(defdefs: List[DefDef])(using ste: StackTraceElement): Option[PrettyStackTraceElement] =
      val decoded = NameTransformer.decode(Names.termName(ste.getMethodName)).toString
      decoded match
        case d if d.contains("$anonfun$") =>
          val lambdas = defdefs.filter(f => f.name == "$anonfun" && f.pos.endLine + 1 == ste.getLineNumber)
          lambdas match
            case head :: Nil =>
              Some(createPrettyStackTraceElement(head, head.pos.startLine + 1))
            case _ =>
              Some(createErrorWhileBrowsingTastyFiles(PrettyErrors.InlinedLambda))
        case d =>
          defdefs.filter(_.name != "$anonfun") match
            case Nil =>
              None
            case head :: Nil =>
              Some(createPrettyStackTraceElement(head, ste.getLineNumber))
            case _ => 
              val fun = defdefs.filter(_.name == d)
              fun match // This will probably fail for nested inline functions, though we cannot disambiguate them
                case head :: Nil =>
                  Some(createPrettyStackTraceElement(head, ste.getLineNumber))
                case defdefs =>
                  Some(createErrorWhileBrowsingTastyFiles(PrettyErrors.Unknown))
       

    st.foreach { ste =>
      tastys.find(_.path.stripSuffix(".class") endsWith ctp(ste.getClassName)) match 
        case Some(tasty) =>
          given StackTraceElement = ste
          val tree = tasty.ast
          val defdefs = walkInOrder(tree)
          processDefDefs(defdefs) match
            case Some(e) => prettyStackTraceElements += e
            case None => // do nothing
        case None =>
          prettyStackTraceElements += PrettyStackTraceElement(ste, ElementType.Method, ste.getMethodName, ste.getClassName, ste.getLineNumber, isTasty = false)
    }
