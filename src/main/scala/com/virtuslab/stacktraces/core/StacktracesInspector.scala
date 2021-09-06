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
    stacktracesInspector.prettyStackTraceElements.reverse.toList


class StacktracesInspector private (st: List[StackTraceElement], ctp: Map[String, String]) extends Inspector:
  private val prettyStackTraceElements: ListBuffer[PrettyStackTraceElement] = ListBuffer.empty
  
  extension (ste: StackTraceElement)
    def getKey: (String, Int) = ste.getClassName -> ste.getLineNumber

  override def inspect(using q: Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import q.reflect.*

    case class Context(
      lambdaUnraveler: LambdaUnraveler,
      defdefs: Map[(String, Int), DefsTree]
    ) {
      def updatedDefTreeStart(k: (String, Int), newStart: Tree) =
        this.copy(
          defdefs = defdefs.updated(
            k,
            defdefs(k).copy(start = Some(newStart.symbol))
          )
        )
    }

    case class DefsTree(
      defdefs: Map[Symbol, List[DefDef]],
      start: Option[Symbol] = None,
      originalStart: Option[Symbol] = None,
    ) {
      def updated(k: Symbol, v: List[DefDef]): DefsTree = 
        this.copy(
          start = start.orElse(Some(k)),
          originalStart = originalStart.orElse(Some(k)),
          defdefs = defdefs.updated(k, v)
        )
    }

    object DefsTree:
      def empty: DefsTree = DefsTree(Map.empty)

    val ts = TypesSupport(q)

    def label(d: DefDef): ElementType =  d.symbol match
      case s if s.flags.is(Flags.ExtensionMethod) => ElementType.ExtensionMethod
      case s if s.name == "$anonfun" => 
        
        val ownerName = s.owner.name
        val parent = if ownerName == "$anonfun" then "some outer lambda" else ownerName
        ElementType.Lambda(ts.toLambda(d.asInstanceOf[ts.qctx.reflect.DefDef]), parent)
      case _ => ElementType.Method
          
    def createPrettyStackTraceElement(d: DefDef, lineNumber: Int)(using ste: StackTraceElement): PrettyStackTraceElement =
      val nameWithoutPrefix = d.pos.sourceFile.getJPath.map(_.toString.stripPrefix("out/bootstrap/stdlib-bootstrapped/scala-3.0.3-RC1-bin-SNAPSHOT-nonbootstrapped/src_managed/main/scala-library-src/")) // TODO: Remove when stdlib will be shipped with tasty files!
      PrettyStackTraceElement(ste, label(d), d.name, nameWithoutPrefix.getOrElse("<TODO: fix>"), lineNumber)

    def createErrorWhileBrowsingTastyFiles(error: PrettyErrors)(using ste: StackTraceElement): PrettyStackTraceElement =
      PrettyStackTraceElement(ste, ElementType.Method, ste.getMethodName, ste.getFileName, ste.getLineNumber, error = Some(error))

    class Traverser(ste: StackTraceElement) extends TreeAccumulator[DefsTree]:
      def foldTree(defdefs: DefsTree, tree: Tree)(owner: Symbol): DefsTree =
        val defdef: Option[DefDef] = tree match 
          case d: DefDef => 
            if d.pos.startLine + 1 <= ste.getLineNumber && d.pos.endLine + 1 >= ste.getLineNumber then
              Some(d)
            else 
              None
          case tree =>
            None

        // TODO: Remove when compiler will fix the issue https://github.com/lampepfl/dotty/issues/13352
        val exists = try 
          tree.pos.startLine
          true
        catch
          case _ => 
            false

        if exists && tree.pos.startLine < ste.getLineNumber then 
          val actual: List[DefDef] = defdefs.defdefs.get(owner).toList.flatten
          val newOwner = if defdef.isEmpty then owner else tree.symbol
          if defdef.nonEmpty then
            foldOverTree(defdefs.updated(owner, actual ++ defdef.toList), tree)(newOwner)
          else
            foldOverTree(defdefs, tree)(newOwner)
        else 
          defdefs
    end Traverser

    def processDefDefs(optionalName: Option[String] = None)(using ctx: Context, ste: StackTraceElement): (Option[PrettyStackTraceElement], Context) =
      val actualDefTree: DefsTree = ctx.defdefs(ste.getKey)
      // actualDefTree.defdefs.foreach { case (sym, trees) =>
      //   println(sym)
      //   trees.foreach { t =>
      //     println(s"    ${t.symbol}")
      //   }
      // }
      val actualTrees: List[DefDef] = actualDefTree.defdefs.get(actualDefTree.start.get).toList.flatten
      val decoded = NameTransformer.decode(Names.termName(optionalName.getOrElse(ste.getMethodName))).toString
      decoded match
        case d if d.contains("$anonfun$") =>
          println(actualTrees.map(_.symbol))
          actualTrees match
            case head :: Nil if head.name.contains("$anonfun") =>
              (Some(createPrettyStackTraceElement(head, ste.getLineNumber)), ctx.updatedDefTreeStart(ste.getKey, head))
            case head :: Nil =>
              processDefDefs()(using ctx.updatedDefTreeStart(ste.getKey, head), ste)
            case _ =>
              (Some(createErrorWhileBrowsingTastyFiles(PrettyErrors.InlinedLambda)), ctx)
        case d =>
          actualTrees match
            case Nil =>
              (None, ctx)
            case head :: Nil if actualDefTree.defdefs.size == 1 =>
              (Some(createPrettyStackTraceElement(head, ste.getLineNumber)), ctx.updatedDefTreeStart(ste.getKey, head))
            case defs => 
              val funs = defs.filter(_.name == decoded)
              funs match // This will probably fail for nested inline functions, though we cannot disambiguate them
                case head :: Nil =>
                  (Some(createPrettyStackTraceElement(head, ste.getLineNumber)), ctx.updatedDefTreeStart(ste.getKey, head))
                case _ =>
                  val extraSuffix = """(.*)\$(\d*)""".r
                  decoded match
                    case extraSuffix(name, suffix) =>
                      processDefDefs(Some(name))
                    case _ =>
                      (Some(createErrorWhileBrowsingTastyFiles(PrettyErrors.Unknown)), ctx)
              
    def processStackTrace(ste: StackTraceElement)(using ctx: Context): Context =
      ctx.defdefs.get(ste.getKey) match 
        case Some(defsTree) =>
          given StackTraceElement = ste
          val (pse, newContext) = processDefDefs()
          pse match
            case Some(e) => prettyStackTraceElements += e
            case None => // do nothing
          newContext
        case None =>
          prettyStackTraceElements += PrettyStackTraceElement(ste, ElementType.Method, ste.getMethodName, ste.getClassName, ste.getLineNumber, isTasty = false)
          ctx
    
    def foreachStacktrace(st: List[StackTraceElement])(using Context): Unit =
      val ste :: tail = st
      val newContext = processStackTrace(ste)
      tail match
        case Nil => // do nothing
        case _ => foreachStacktrace(tail)(using newContext)

    //TODO optimize
    def readDefsTree(stes: List[StackTraceElement]): Map[(String, Int), DefsTree] =
      stes.distinctBy(ste => ste.getKey).foldLeft(Map.empty) { case (acc, ste) =>
        tastys.find(_.path.stripSuffix(".class") endsWith ctp(ste.getClassName)) match
          case Some(tasty) =>
            given StackTraceElement = ste
            val tree = tasty.ast
            val traverser = Traverser(ste)
            val defdefs = traverser.foldTree(DefsTree.empty, tree)(tree.symbol)
            acc.updated(ste.getKey, defdefs)
          case None =>
            prettyStackTraceElements += PrettyStackTraceElement(ste, ElementType.Method, ste.getMethodName, ste.getClassName, ste.getLineNumber, isTasty = false)
            acc
      }

    foreachStacktrace(st.reverse)(using Context(LambdaUnraveler(q)(Nil), readDefsTree(st)))
