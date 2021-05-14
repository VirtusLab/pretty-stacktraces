package org.virtuslab.stacktraces

import org.virtuslab.stacktraces.parser.StacktraceParser
import org.virtuslab.stacktraces.core.Stacktraces
import org.virtuslab.stacktraces.printer.PrettyExceptionPrinter

import org.junit.Test
import org.junit.Assert._

val stacktraceRaw = """Test org.virtuslab.stacktraces.tests.BasicTest.nestedLambdas failed: java.lang.RuntimeException: error, took 0.002 sec
    at org.virtuslab.stacktraces.tests.BasicTest$package$.$bang(BasicTest.scala:23)
    at org.virtuslab.stacktraces.tests.BasicTest.$anonfun$1$$anonfun$1$$anonfun$1$$anonfun$1(BasicTest.scala:35)
    at scala.collection.immutable.List.map(List.scala:246)
    at org.virtuslab.stacktraces.tests.BasicTest.$anonfun$2$$anonfun$2$$anonfun$2(BasicTest.scala:35)
    at org.virtuslab.stacktraces.tests.BasicTest.$anonfun$3$$anonfun$3$$anonfun$adapted$1(BasicTest.scala:36)
    at scala.collection.immutable.List.flatMap(List.scala:293)
    at org.virtuslab.stacktraces.tests.BasicTest.$anonfun$4$$anonfun$4(BasicTest.scala:36)
    at org.virtuslab.stacktraces.tests.BasicTest.$anonfun$5$$anonfun$adapted$1(BasicTest.scala:37)
    at scala.collection.immutable.List.map(List.scala:246)
    at org.virtuslab.stacktraces.tests.BasicTest.$anonfun$6(BasicTest.scala:37)
    at org.virtuslab.stacktraces.tests.BasicTest.$anonfun$adapted$1(BasicTest.scala:38)
    at scala.collection.StrictOptimizedIterableOps.flatMap(StrictOptimizedIterableOps.scala:117)
    at scala.collection.StrictOptimizedIterableOps.flatMap$(StrictOptimizedIterableOps.scala:104)
    at scala.collection.immutable.Range.flatMap(Range.scala:59)
    at org.virtuslab.stacktraces.tests.BasicTest.nestedLambdas$$anonfun$1(BasicTest.scala:38)
    at org.virtuslab.stacktraces.util.TestExecutor$.executeTest(TestExecutor.scala:13)
    at org.virtuslab.stacktraces.tests.BasicTest.nestedLambdas(BasicTest.scala:40)
    at jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
    at jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
    at jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
    at java.lang.reflect.Method.invoke(Method.java:566)
"""

class StacktraceParserTest:

  @Test 
  def parserTest =
    val errorOrStacktrace = StacktraceParser.parse(stacktraceRaw)
    errorOrStacktrace match
      case Left(msg) => throw AssertionError(msg)
      case Right(stacktrace) =>
        val prettyStackTrace = Stacktraces.convertToPrettyStackTrace(stacktrace)
        /*
         * This stacktrace is subject to Tasty files generated for BasicTest.scala, if you see some weird output, probably BasicTest.scala diverged.
         * Nonetheless, we can count test as passing correctly if it hits Right branch.
         */
        PrettyExceptionPrinter.printStacktrace(prettyStackTrace)
