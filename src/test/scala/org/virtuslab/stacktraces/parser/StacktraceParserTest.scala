package org.virtuslab.stacktraces
package parser

import org.virtuslab.stacktraces.parser.StacktraceParser
import org.virtuslab.stacktraces.core.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter

import org.junit.Test
import org.junit.Assert.assertTrue
import org.junit.Assume.assumeTrue

val stacktraceRaw = """Test org.virtuslab.stacktraces.parser.TestInput.test failed: java.lang.RuntimeException: TestInput, took 0.004 sec
    at org.virtuslab.stacktraces.parser.TestInput$package$.$bang(TestInput.scala:7)
    at org.virtuslab.stacktraces.parser.TestInput.$anonfun$1$$anonfun$1$$anonfun$1$$anonfun$1(TestInput.scala:18)
    at scala.collection.immutable.List.map(List.scala:246)
    at org.virtuslab.stacktraces.parser.TestInput.$anonfun$2$$anonfun$2$$anonfun$2(TestInput.scala:18)
    at org.virtuslab.stacktraces.parser.TestInput.$anonfun$3$$anonfun$3$$anonfun$adapted$1(TestInput.scala:18)
    at scala.collection.immutable.List.flatMap(List.scala:293)
    at org.virtuslab.stacktraces.parser.TestInput.$anonfun$4$$anonfun$4(TestInput.scala:18)
    at org.virtuslab.stacktraces.parser.TestInput.$anonfun$5$$anonfun$adapted$1(TestInput.scala:19)
    at scala.collection.immutable.List.map(List.scala:246)
    at org.virtuslab.stacktraces.parser.TestInput.$anonfun$6(TestInput.scala:19)
    at org.virtuslab.stacktraces.parser.TestInput.$anonfun$adapted$1(TestInput.scala:20)
    at scala.collection.StrictOptimizedIterableOps.flatMap(StrictOptimizedIterableOps.scala:117)
    at scala.collection.StrictOptimizedIterableOps.flatMap$(StrictOptimizedIterableOps.scala:104)
    at scala.collection.immutable.Range.flatMap(Range.scala:59)
    at org.virtuslab.stacktraces.parser.TestInput.test(TestInput.scala:20)
    at jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
    at jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
    at jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
    at java.lang.reflect.Method.invoke(Method.java:566)
"""

val prettifiedStacktrace = """    at extension method ! in src/test/scala/org/virtuslab/stacktraces/parser/TestInput.scala:7 
    at lambda (String) => Int of some outer lambda in src/test/scala/org/virtuslab/stacktraces/parser/TestInput.scala:18 
    at method map in scala/collection/immutable/List.scala:246 
    at lambda (Boolean) => IterableOnce[Int] of some outer lambda in src/test/scala/org/virtuslab/stacktraces/parser/TestInput.scala:18 
    at method flatMap in scala/collection/immutable/List.scala:293 
    at lambda (Int) => List[Int] of some outer lambda in src/test/scala/org/virtuslab/stacktraces/parser/TestInput.scala:19 
    at method map in scala/collection/immutable/List.scala:246 
    at lambda (Int) => IterableOnce[List[Int]] of x in src/test/scala/org/virtuslab/stacktraces/parser/TestInput.scala:20 
    at method strictOptimizedFlatMap in scala/collection/StrictOptimizedIterableOps.scala:117 
    at method flatMap in scala/collection/StrictOptimizedIterableOps.scala:104 
    at method <init> in scala/collection/immutable/Range.scala:59 
    at method test in src/test/scala/org/virtuslab/stacktraces/parser/TestInput.scala:20 
    at method invoke0 in jdk.internal.reflect.NativeMethodAccessorImpl:(Native method) 
    at method invoke in jdk.internal.reflect.NativeMethodAccessorImpl:62 
    at method invoke in jdk.internal.reflect.DelegatingMethodAccessorImpl:43 
    at method invoke in java.lang.reflect.Method:566 
"""

class StacktraceParserTest:
 
  @Test
  def parserTest =
    assumeTrue(System.getProperty("java.version").startsWith("11."))
    val errorOrStacktrace = StacktraceParser.parse(stacktraceRaw)
    errorOrStacktrace match
      case Left(msg) => throw AssertionError(msg)
      case Right(stacktrace) =>
        val prettyStackTrace = Stacktraces.convertToPrettyStackTrace(stacktrace, Seq("scala-library_3-3.1.0-bin-SNAPSHOT.jar"))
        /*
         * This stacktrace is subject to Tasty files generated for TestInput.scala, if you see some weird output, probably TestInput.scala diverged.
         */
        assertTrue(PrettyExceptionPrinter.prettyStacktrace(prettyStackTrace).build.replaceAll("\u001b\\[[;\\d]*m", "").endsWith(prettifiedStacktrace))
