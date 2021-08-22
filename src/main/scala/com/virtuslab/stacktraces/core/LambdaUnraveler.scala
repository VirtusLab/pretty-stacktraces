package org.virtuslab.stacktraces.core

import scala.quoted.*
import scala.tasty.inspector.*

class LambdaUnraveler private (val q: Quotes)(private val previousLambdas: List[q.reflect.DefDef], private val counter: Int = 0):

  def getNextLambdaAndState(using qd: Quotes)(defdefs: List[qd.reflect.DefDef]): (Option[qd.reflect.DefDef], LambdaUnraveler) =
    if defdefs.nonEmpty && previousLambdas == defdefs then
      (Some(defdefs.reverse(counter)), new LambdaUnraveler(q)(previousLambdas, counter + 1))
    else if defdefs.nonEmpty then
      (Some(defdefs.last), new LambdaUnraveler(qd)(defdefs, 1))
    else
      (None, new LambdaUnraveler(qd)(defdefs, 0))


object LambdaUnraveler:
  def apply(q: Quotes)(defdefs: List[q.reflect.DefDef]): LambdaUnraveler =
    new LambdaUnraveler(q)(defdefs, 0)
