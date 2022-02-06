package org.virtuslab.stacktraces.transform

import org.virtuslab.stacktraces.model._

import scala.util.chaining._

object StacktracesCompresser:
  def compressStackTrace(st: List[PrettyStackTraceElement]): List[(Int, List[PrettyStackTraceElement])] =
    goCompress(st)
      .pipe(concat)

  private def concat(stackTraceParts: List[(Int, List[PrettyStackTraceElement])]): List[(Int, List[PrettyStackTraceElement])] =
    stackTraceParts.flatMap {
      case (_, Nil) => Nil
      case p => List(p)
    }

  private def goCompress(
    left: List[PrettyStackTraceElement],
    lastPartCount: Int = 1,
    lastPart: List[PrettyStackTraceElement] = List.empty,
    res: List[(Int, List[PrettyStackTraceElement])] = List.empty
  ): List[(Int, List[PrettyStackTraceElement])] =
    left match
      case Nil =>
        res :+ (lastPartCount, lastPart)
      case (h :: tail) =>
        if lastPart.contains(h) then
          if lastPartCount == 1 then
            val bestMatch =
              0.until(lastPart.size).flatMap { (i: Int) =>
                val (skipped, prevFrame) = lastPart.splitAt(i)
                val (frameFromLeft, afterFrame) = left.splitAt(prevFrame.size)
                val framesEqual = frameFromLeft.size == prevFrame.size && frameFromLeft.zip(prevFrame).forall { case (e1, e2) => e1 == e2 }
                if framesEqual then
                  Some((skipped, prevFrame, frameFromLeft, afterFrame))
                else
                  None
              }.headOption
            bestMatch match
              case None =>
                goCompress(tail, lastPartCount = 1, lastPart = List(h), res :+ (lastPartCount, lastPart))
              case Some((skipped, prevFrame, frameFromLeft, afterFrame)) =>
                goCompress(afterFrame, lastPartCount = 2, lastPart = prevFrame, res :+ (lastPartCount, skipped))
          else
            val (frameFromLeft, afterFrame) = left.splitAt(lastPart.size)
            val framesEqual = frameFromLeft.size == lastPart.size && frameFromLeft.zip(lastPart).forall { case (e1, e2) => e1 == e2 }
            if framesEqual then
              goCompress(afterFrame, lastPartCount = lastPartCount + 1, lastPart = lastPart, res)
            else
              goCompress(tail, lastPartCount = 1, lastPart = List(h), res :+ (lastPartCount, lastPart))
        else if lastPartCount == 1 then
          goCompress(tail, lastPartCount = 1, lastPart = lastPart :+ h, res)
        else
          goCompress(tail, lastPartCount = 1, lastPart = List(h), res :+ (lastPartCount, lastPart))
