package com.iamsmkr.chesstour.heuristic

import scala.annotation.tailrec

// Based on Warnsdorff Heuristic Algorithm

object ChessTour extends App {

  case class Pos(i: Int, j: Int)

  case class Stats(alreadyVisitedPos: List[Pos], maybeNextPossiblePos: Option[Pos], otherPossiblePos: List[Pos])

  val allPos = for {x <- 0 until 10; y <- 0 until 10} yield Pos(x, y)

  def solve(currPos: Pos): List[Pos] = {

    def getPossiblePos(currPos: Pos): List[Pos] = {
      val Pos(i, j) = currPos

      List(
        Pos(i - 2, j - 2), Pos(i - 3, j), Pos(i - 2, j + 2),
        Pos(i, j - 3), Pos(i, j + 3),
        Pos(i + 2, j - 2), Pos(i + 3, j), Pos(i + 2, j + 2)
      ).filter { case Pos(x, y) =>
        x >= 0 && x <= 9 && y >= 0 && y <= 9
      }
    }

    def getNextPossiblePosWithLeastFurtherPossiblePos(nextPossiblePos: List[Pos], alreadyVisitedPos: List[Pos]): Option[Pos] = {
      if (nextPossiblePos.isEmpty) return None
      Some(nextPossiblePos.map { n =>
        (n, getPossiblePos(n) diff alreadyVisitedPos)
      }.minBy { case (_, p) => p.size }._1)
    }

    @tailrec
    def getStatsWithOtherPossiblePos(stats: List[Stats]): List[Stats] = {
      if (stats.head.otherPossiblePos.isEmpty) getStatsWithOtherPossiblePos(stats.tail) else {

        val Stats(alreadyVisitedPos, _, otherPossiblePos) = stats.head
        Stats(
          alreadyVisitedPos,
          otherPossiblePos.headOption,
          if (otherPossiblePos.size > 1) otherPossiblePos.tail else Nil) :: stats.tail
      }
    }

    @tailrec
    def compute(stats: List[Stats]): List[Pos] = {
      val currStats = stats.head

      if ((allPos diff currStats.alreadyVisitedPos).isEmpty) currStats.alreadyVisitedPos.reverse
      else {
        if (currStats.maybeNextPossiblePos.isDefined) {
          val nextPos = currStats.maybeNextPossiblePos.get

          val alreadyVisitedPos = nextPos :: currStats.alreadyVisitedPos
          val possiblePos = getPossiblePos(nextPos) diff alreadyVisitedPos
          val maybeNextPossiblePos = getNextPossiblePosWithLeastFurtherPossiblePos(possiblePos, alreadyVisitedPos)
          val otherPossiblePos = if (possiblePos.size > 1) possiblePos.filter(_ != maybeNextPossiblePos.get) else Nil

          val newStats = Stats(alreadyVisitedPos, maybeNextPossiblePos, otherPossiblePos)
          compute(newStats :: stats)
        } else
          compute(getStatsWithOtherPossiblePos(stats.tail))
      }
    }

    val possiblePos = getPossiblePos(currPos)
    val alreadyVisitedPos = List(currPos)
    val maybeNextPossiblePos = getNextPossiblePosWithLeastFurtherPossiblePos(possiblePos, alreadyVisitedPos)
    val otherPossiblePos = if (possiblePos.size > 1) possiblePos.filter(_ != maybeNextPossiblePos.get) else Nil

    val stats = List(Stats(alreadyVisitedPos, maybeNextPossiblePos, otherPossiblePos))
    compute(stats)
  }

  println(solve(Pos(4, 3)))
}

object ChessTour3X3 extends App {

  case class Pos(i: Int, j: Int)

  case class Stats(alreadyVisitedPos: List[Pos], maybeNextPossiblePos: Option[Pos], otherPossiblePos: List[Pos])

  val allPos = for {x <- 0 until 3; y <- 0 until 3} yield Pos(x, y)

  def solve(currPos: Pos): List[Pos] = {

    def getPossiblePos(currPos: Pos): List[Pos] = {
      val Pos(i, j) = currPos

      List(
        Pos(i - 1, j - 1), Pos(i - 1, j), Pos(i - 1, j + 1),
        Pos(i, j - 1), Pos(i, j + 1),
        Pos(i + 1, j - 1), Pos(i + 1, j), Pos(i + 1, j + 1)
      ).filter { case Pos(x, y) =>
        x >= 0 && x <= 2 && y >= 0 && y <= 2
      }
    }

    def getNextPossiblePosWithLeastFurtherPossiblePos(nextPossiblePos: List[Pos], alreadyVisitedPos: List[Pos]): Option[Pos] = {
      if (nextPossiblePos.isEmpty) return None
      Some(nextPossiblePos.map { n =>
        (n, getPossiblePos(n) diff alreadyVisitedPos)
      }.minBy { case (_, p) => p.size }._1)
    }

    @tailrec
    def getStatsWithOtherPossiblePos(stats: List[Stats]): List[Stats] = {
      if (stats.head.otherPossiblePos.isEmpty) getStatsWithOtherPossiblePos(stats.tail) else {

        val Stats(alreadyVisitedPos, _, otherPossiblePos) = stats.head
        Stats(
          alreadyVisitedPos,
          otherPossiblePos.headOption,
          if (otherPossiblePos.size > 1) otherPossiblePos.tail else Nil) :: stats.tail
      }
    }

    @tailrec
    def compute(stats: List[Stats]): List[Pos] = {
      val currStats = stats.head

      if ((allPos diff currStats.alreadyVisitedPos).isEmpty) currStats.alreadyVisitedPos.reverse
      else {
        if (currStats.maybeNextPossiblePos.isDefined) {
          val nextPos = currStats.maybeNextPossiblePos.get

          val alreadyVisitedPos = nextPos :: currStats.alreadyVisitedPos
          val possiblePos = getPossiblePos(nextPos) diff alreadyVisitedPos
          val maybeNextPossiblePos = getNextPossiblePosWithLeastFurtherPossiblePos(possiblePos, alreadyVisitedPos)
          val otherPossiblePos = if (possiblePos.size > 1) possiblePos.filter(_ != maybeNextPossiblePos.get) else Nil

          val newStats = Stats(alreadyVisitedPos, maybeNextPossiblePos, otherPossiblePos)
          compute(newStats :: stats)
        } else
          compute(getStatsWithOtherPossiblePos(stats.tail))
      }
    }

    val possiblePos = getPossiblePos(currPos)
    val alreadyVisitedPos = List(currPos)
    val maybeNextPossiblePos = getNextPossiblePosWithLeastFurtherPossiblePos(possiblePos, alreadyVisitedPos)
    val otherPossiblePos = if (possiblePos.size > 1) possiblePos.filter(_ != maybeNextPossiblePos.get) else Nil

    val stats = List(Stats(alreadyVisitedPos, maybeNextPossiblePos, otherPossiblePos))
    compute(stats)
  }

  assert(solve(Pos(0, 0)) == List(Pos(0, 0), Pos(0, 1), Pos(0, 2), Pos(1, 2), Pos(2, 2), Pos(1, 1), Pos(1, 0), Pos(2, 0), Pos(2, 1)))
  assert(solve(Pos(1, 1)) == List(Pos(1, 1), Pos(0, 0), Pos(0, 1), Pos(0, 2), Pos(1, 2), Pos(2, 2), Pos(2, 1), Pos(1, 0), Pos(2, 0)))
  assert(solve(Pos(2, 2)) == List(Pos(2, 2), Pos(1, 2), Pos(0, 2), Pos(0, 1), Pos(0, 0), Pos(1, 0), Pos(1, 1), Pos(2, 0), Pos(2, 1)))
  assert(solve(Pos(2, 0)) == List(Pos(2, 0), Pos(1, 0), Pos(0, 0), Pos(0, 1), Pos(0, 2), Pos(1, 1), Pos(1, 2), Pos(2, 1), Pos(2, 2)))
}
