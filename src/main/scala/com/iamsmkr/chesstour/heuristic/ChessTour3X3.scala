package com.iamsmkr.chesstour.heuristic

import scala.annotation.tailrec

// Simplified problem statement for ease of implementation and testing.
// Same implementation could be extended to chess boards of higher dimensions.

object ChessTour3X3 extends App {

  case class Pos(i: Int, j: Int)

  // Stats hold relevent information to facilitate tail recursive backtracking for every position in a given board.

  case class Stats(alreadyVisitedPos: List[Pos], maybeNextPossiblePos: Option[Pos], otherPossiblePos: List[Pos])

  def solve(currPos: Pos): List[Pos] = {

    val allPos = for {x <- 0 until 3; y <- 0 until 3} yield Pos(x, y)

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

    // Get next possible position with least further possible positions available to move to out of all next available
    // possible positions.

    def getNextPossiblePos(nextPossiblePos: List[Pos], alreadyVisitedPos: List[Pos]): (Option[Pos], List[Pos]) = {
      if (nextPossiblePos.isEmpty) return (None, Nil)

      val leastPos = nextPossiblePos.map { n => (n, getPossiblePos(n) diff alreadyVisitedPos) }.minBy { case (_, p) => p.size }

      val maybeNextPossiblePos = Some(leastPos._1)
      val otherPossiblePos = leastPos._2.filter(_ != maybeNextPossiblePos.get)

      (maybeNextPossiblePos, otherPossiblePos)
    }

    // Get last stats in the stack that has non empty other possible positions to move to because there is no way to continue
    // further with empty other possible positions.

    @tailrec
    def getLastStats(stats: List[Stats]): List[Stats] = {
      if (stats.head.otherPossiblePos.isEmpty) getLastStats(stats.tail) else {

        val Stats(alreadyVisitedPos, _, _otherPossiblePos) = stats.head
        val (maybeNextPossiblePos, otherPossiblePos) = getNextPossiblePos(_otherPossiblePos, alreadyVisitedPos)

        Stats(alreadyVisitedPos, maybeNextPossiblePos, otherPossiblePos) :: stats.tail
      }
    }

    @tailrec
    def compute(stats: List[Stats]): List[Pos] = {
      if (stats.isEmpty) Nil
      else {
        val currStats = stats.head

        val unvisitedPos = allPos diff currStats.alreadyVisitedPos

        if (unvisitedPos.isEmpty) currStats.alreadyVisitedPos.reverse
        else {
          if (currStats.maybeNextPossiblePos.isDefined) {
            val nextPos = currStats.maybeNextPossiblePos.get

            val alreadyVisitedPos = nextPos :: currStats.alreadyVisitedPos
            val possiblePos = getPossiblePos(nextPos) diff alreadyVisitedPos
            val (maybeNextPossiblePos, otherPossiblePos) = getNextPossiblePos(possiblePos, alreadyVisitedPos)

            val newStats = Stats(alreadyVisitedPos, maybeNextPossiblePos, otherPossiblePos)
            compute(newStats :: stats)
          } else compute(getLastStats(stats.tail))
        }
      }
    }

    val possiblePos = getPossiblePos(currPos)
    val alreadyVisitedPos = List(currPos)
    val (maybeNextPossiblePos, otherPossiblePos) = getNextPossiblePos(possiblePos, alreadyVisitedPos)

    val stats = List(Stats(alreadyVisitedPos, maybeNextPossiblePos, otherPossiblePos))
    compute(stats)
  }

}
