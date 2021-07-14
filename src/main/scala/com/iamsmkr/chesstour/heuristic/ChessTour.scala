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
          if (otherPossiblePos.size > 1) otherPossiblePos.tail else Nil
        ) :: stats.tail
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
          val otherPossiblePos = possiblePos.filter(_ != maybeNextPossiblePos.get)

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

  //  println(solve(Pos(4, 3)))

  assert(
    solve(Pos(4, 3)) ==
      List(
        Pos(4, 3), Pos(2, 1), Pos(0, 3), Pos(0, 0), Pos(3, 0), Pos(1, 2), Pos(1, 5), Pos(1, 8), Pos(4, 8), Pos(7, 8), Pos(9, 6), Pos(9, 9), Pos(6, 9), Pos(8, 7),
        Pos(8, 4), Pos(8, 1), Pos(5, 1), Pos(3, 3), Pos(1, 1), Pos(1, 4), Pos(1, 7), Pos(3, 9), Pos(0, 9), Pos(0, 6), Pos(3, 6), Pos(6, 6), Pos(8, 8), Pos(5, 8),
        Pos(2, 8), Pos(2, 5), Pos(0, 7), Pos(2, 9), Pos(4, 7), Pos(7, 7), Pos(5, 9), Pos(8, 9), Pos(8, 6), Pos(6, 8), Pos(9, 8), Pos(9, 5), Pos(6, 5), Pos(8, 3),
        Pos(8, 0), Pos(5, 0), Pos(2, 0), Pos(0, 2), Pos(0, 5), Pos(0, 8), Pos(3, 8), Pos(1, 6), Pos(1, 9), Pos(3, 7), Pos(5, 5), Pos(7, 3), Pos(9, 1), Pos(6, 1),
        Pos(3, 1), Pos(0, 1), Pos(0, 4), Pos(2, 2), Pos(4, 0), Pos(1, 0), Pos(1, 3), Pos(3, 5), Pos(3, 2), Pos(6, 2), Pos(9, 2), Pos(7, 0), Pos(5, 2), Pos(3, 4),
        Pos(5, 6), Pos(2, 6), Pos(4, 4), Pos(7, 4), Pos(7, 1), Pos(4, 1), Pos(2, 3), Pos(5, 3), Pos(7, 5), Pos(9, 3), Pos(9, 0), Pos(6, 0), Pos(6, 3), Pos(8, 5),
        Pos(8, 2), Pos(6, 4), Pos(4, 2), Pos(4, 5), Pos(6, 7), Pos(9, 7), Pos(9, 4), Pos(7, 2), Pos(5, 4), Pos(2, 4), Pos(2, 7), Pos(5, 7), Pos(7, 9), Pos(4, 9),
        Pos(4, 6), Pos(7, 6)
      )
  )
}

// Simplified problem statement for ease of implementation and testing.
// Same implementation could be extended to chess boards of higher dimensions.

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
          if (otherPossiblePos.size > 1) otherPossiblePos.tail else Nil
        ) :: stats.tail
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
          val otherPossiblePos = possiblePos.filter(_ != maybeNextPossiblePos.get)

          val newStats = Stats(alreadyVisitedPos, maybeNextPossiblePos, otherPossiblePos)
          compute(newStats :: stats)
        } else
          compute(getStatsWithOtherPossiblePos(stats.tail))
      }
    }

    val possiblePos = getPossiblePos(currPos)
    val alreadyVisitedPos = List(currPos)
    val maybeNextPossiblePos = getNextPossiblePosWithLeastFurtherPossiblePos(possiblePos, alreadyVisitedPos)
    val otherPossiblePos = possiblePos.filter(_ != maybeNextPossiblePos.get)

    val stats = List(Stats(alreadyVisitedPos, maybeNextPossiblePos, otherPossiblePos))
    compute(stats)
  }

  assert(solve(Pos(0, 0)) == List(Pos(0, 0), Pos(0, 1), Pos(0, 2), Pos(1, 2), Pos(2, 2), Pos(1, 1), Pos(1, 0), Pos(2, 0), Pos(2, 1)))
  assert(solve(Pos(1, 1)) == List(Pos(1, 1), Pos(0, 0), Pos(0, 1), Pos(0, 2), Pos(1, 2), Pos(2, 2), Pos(2, 1), Pos(1, 0), Pos(2, 0)))
  assert(solve(Pos(2, 2)) == List(Pos(2, 2), Pos(1, 2), Pos(0, 2), Pos(0, 1), Pos(0, 0), Pos(1, 0), Pos(1, 1), Pos(2, 0), Pos(2, 1)))
  assert(solve(Pos(2, 0)) == List(Pos(2, 0), Pos(1, 0), Pos(0, 0), Pos(0, 1), Pos(0, 2), Pos(1, 1), Pos(1, 2), Pos(2, 1), Pos(2, 2)))
}
