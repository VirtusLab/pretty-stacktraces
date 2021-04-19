package com.virtuslab.stacktracebuddy

import dotty.tools.dotc.util.NameTransformer
import dotty.tools.dotc.core.Names

import scala.quoted.*
import scala.tasty.inspector.*
import scala.collection.JavaConverters.*

import java.io.File
import java.nio.file.Paths

object StackTraceBuddy:

  private val rootPath = Paths.get("target", "scala-3.0.0-RC2", "test-classes")

  def convertToPrettyStackTrace(e: Exception): PrettyException = 
    val st = e.getStackTrace.map { ste =>
      val classFile = ste.getClassName
      val classPath = classFile.stripSuffix("$").split("\\.")
      
      val filePath = Paths.get(rootPath.toString, classPath*)
      val tastyPath = filePath.resolveSibling(filePath.getFileName.toString + ".tasty")
      val tastyFile = tastyPath.toFile

      
      if tastyFile.exists then
        val stackTraceBuddyInspector = StackTraceBuddyInspector(ste)
        TastyInspector.inspectTastyFiles(List(tastyFile.toString))(stackTraceBuddyInspector)
        stackTraceBuddyInspector.prettyStackTrace
      else
        PrettyStackTrace(ste, "")
    }.toList
    PrettyException(e, st)

  private class StackTraceBuddyInspector(ste: StackTraceElement) extends Inspector:
    var prettyStackTrace: PrettyStackTrace = null
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      import quotes.reflect.*

      for tasty <- tastys do
        val tree = tasty.ast
        matchTree(tree)
            
        // println(tree.show(using Printer.TreeStructure))
      
      def matchTree(tree: Tree): Unit =
        tree match
          case PackageClause(_, list) => list.foreach(matchTree)
          case ClassDef(_, _, _, _, list) => list.foreach(matchTree)
          case d @ DefDef(name, _, _, rhs) => 
            val decoded = NameTransformer.decode(Names.termName(ste.getMethodName))

            // println((name, ste.getMethodName.split("\\$").head, ste))

            if name == decoded.toString then
              println(label(d))
              println(d.pos.sourceFile.jpath)
              prettyStackTrace = PrettyStackTrace(ste, label(d))
            else if name == "stacktraceTest" then
              matchTerm(rhs.get)
            else if name == "$anonfun" then
              println(d.pos.startLine)
              println(d.pos.endLine)
              // println(d)

          case ValDef(_, _, rhs) => 
            matchTerm(rhs.get)
          case _ =>
            List(tree)
      
      def matchTerm(term: Term): Unit = term match
        case a @ Try(body, _, _) => 
          // println(a)
          matchTerm(body)
        case Block(list, term) => 
          list.foreach(matchTree)
          matchTerm(term)
        case Apply(term, list) => 
          matchTerm(term)
          list.foreach(matchTerm)
        case TypeApply(term, _) => matchTerm(term)
        case Select(term, _) => matchTerm(term)
        case x => 


      def label(tree: Tree): String =  tree.symbol match
        case s if s.flags.is(Flags.ExtensionMethod) => "extension method"
        case _ => "method"





  // private def getTastyFiles(): List[String] =
  //   def getAllFiles(f: File): LazyList[File] =
  //     f #:: (if f.isDirectory then f.listFiles().to(LazyList).flatMap(getAllFiles) else LazyList.empty)
  //   getAllFiles(File("target/scala-3.0.0-RC2/test-classes/")).filter(_.getName.endsWith(".tasty")).map(_.toPath.toAbsolutePath.toString).toList
