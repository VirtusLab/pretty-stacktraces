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
    val tastys = tastyFiles.map(_.file.toPath.toString)
    val dependencies = tastyFiles.flatMap(_.jar).map(_.toPath.toString)
    TastyInspector.inspectAllTastyFiles(tastys, Nil, dependencies)(stacktracesInspector)
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
      val nameWithoutPrefix = d.pos.sourceFile.getJPath.map(_.toString.stripPrefix("out/bootstrap/stdlib-bootstrapped/scala-3.1.3-RC2-bin-SNAPSHOT-nonbootstrapped/src_managed/main/scala-library-src/")) // TODO: Remove when stdlib will be shipped with tasty files!
      PrettyStackTraceElement(ste, label(d), d.name, nameWithoutPrefix.getOrElse("<TODO: fix>"), lineNumber)

    def createErrorWhileBrowsingTastyFiles(error: PrettyErrors)(using ste: StackTraceElement): PrettyStackTraceElement =
      PrettyStackTraceElement(ste, ElementType.Method, ste.getMethodName, ste.getFileName, ste.getLineNumber, error = Some(error))

    class Traverser(ste: StackTraceElement) extends TreeAccumulator[List[DefDef]]:
      def foldTree(defdefs: List[DefDef], tree: Tree)(owner: Symbol): List[DefDef] =
        val defdef = tree match 
          case d: DefDef => 
            if d.pos.startLine + 1 <= ste.getLineNumber && d.pos.endLine + 1 >= ste.getLineNumber then
              List(d)
            else 
              Nil
          case tree =>
            Nil

        // TODO: Remove when compiler will fix the issue https://github.com/lampepfl/dotty/issues/13352
        val exists = try 
          tree.pos.startLine
          true
        catch
          case _ => 
            false

        if exists && tree.pos.startLine < ste.getLineNumber then 
          try
            foldOverTree(defdefs ++ defdef, tree)(owner)
          catch
            case e: scala.MatchError =>
              defdefs
        else 
          defdefs
    end Traverser

    def processDefDefs(
      defdefs: List[DefDef],
      optionalName: Option[String] = None
    )(
      using lambdaUnraveler: LambdaUnraveler, 
      ste: StackTraceElement
    ): (Option[PrettyStackTraceElement], LambdaUnraveler) =
      val decoded = NameTransformer.decode(Names.termName(optionalName.getOrElse(ste.getMethodName))).toString
      decoded match
        case d if d.contains("$anonfun$") =>
          val lambdas = defdefs.filter(f => f.name == "$anonfun")
          val (defdef, newLambdaUnraveler) = lambdaUnraveler.getNextLambdaAndState(lambdas, decoded)
          defdef match
            case Some(head) =>
              (Some(createPrettyStackTraceElement(head, ste.getLineNumber)), newLambdaUnraveler)
            case None =>
              (Some(createErrorWhileBrowsingTastyFiles(PrettyErrors.InlinedLambda)), newLambdaUnraveler)
        case d =>
          defdefs.filter(_.name != "$anonfun") match
            case Nil =>
              (None, lambdaUnraveler)
            case head :: Nil =>
              (Some(createPrettyStackTraceElement(head, ste.getLineNumber)), lambdaUnraveler)
            case _ => 
              val fun = defdefs.filter(_.name == d)
              fun match // This will probably fail for nested inline functions, though we cannot disambiguate them
                case head :: Nil =>
                  (Some(createPrettyStackTraceElement(head, ste.getLineNumber)), lambdaUnraveler)
                case _ =>
                  val extraSuffix = """(.*)\$(\d*)""".r
                  decoded match
                    case extraSuffix(name, suffix) =>
                      processDefDefs(defdefs, Some(name))
                    case _ =>
                      (Some(createErrorWhileBrowsingTastyFiles(PrettyErrors.Unknown)), lambdaUnraveler)
              
    def processStackTrace(ste: StackTraceElement)(using LambdaUnraveler): LambdaUnraveler =
      tastys.find(_.path.stripSuffix(".class") endsWith ctp(ste.getClassName)) match 
        case Some(tasty) =>
          given StackTraceElement = ste
          val tree = tasty.ast
          val traverser = Traverser(ste)
          val defdefs = traverser.foldTree(List.empty, tree)(tree.symbol)
          val (pse, newLambdaUnraveler) = processDefDefs(defdefs)
          pse match
            case Some(e) => prettyStackTraceElements += e
            case None => // do nothing
          newLambdaUnraveler
        case None =>
          prettyStackTraceElements += PrettyStackTraceElement(ste, ElementType.Method, ste.getMethodName, ste.getClassName, ste.getLineNumber, isTasty = false)
          summon[LambdaUnraveler]
    
    def foreachStacktrace(st: List[StackTraceElement])(using LambdaUnraveler): Unit =
      val ste :: tail = st
      val newLambdaUnraveler = processStackTrace(ste)
      tail match
        case Nil => // do nothing
        case _ => foreachStacktrace(tail)(using newLambdaUnraveler)

    foreachStacktrace(st)(using LambdaUnraveler(q)(Nil))
