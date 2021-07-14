package com.iamsmkr.chesstour.heuristic

import com.iamsmkr.chesstour.heuristic.ChessTour.Pos
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ChessTourSpec extends AnyPropSpec with should.Matchers with ScalaCheckPropertyChecks {
  property("ChessTour should be computed a tour for a given starting position") {
    val results =
      Table(
        ("pos", "result"),
        (Pos(4, 3), List(
          Pos(4, 3), Pos(2, 1), Pos(0, 3), Pos(0, 0), Pos(3, 0), Pos(1, 2), Pos(1, 5), Pos(1, 8), Pos(4, 8), Pos(7, 8),
          Pos(9, 6), Pos(9, 9), Pos(6, 9), Pos(8, 7), Pos(8, 4), Pos(8, 1), Pos(5, 1), Pos(3, 3), Pos(1, 1), Pos(1, 4),
          Pos(1, 7), Pos(3, 9), Pos(0, 9), Pos(0, 6), Pos(3, 6), Pos(6, 6), Pos(8, 8), Pos(5, 8), Pos(2, 8), Pos(2, 5),
          Pos(0, 7), Pos(2, 9), Pos(4, 7), Pos(7, 7), Pos(5, 9), Pos(8, 9), Pos(8, 6), Pos(6, 8), Pos(9, 8), Pos(9, 5),
          Pos(6, 5), Pos(8, 3), Pos(8, 0), Pos(5, 0), Pos(2, 0), Pos(0, 2), Pos(0, 5), Pos(0, 8), Pos(3, 8), Pos(1, 6),
          Pos(1, 9), Pos(3, 7), Pos(5, 5), Pos(7, 3), Pos(9, 1), Pos(6, 1), Pos(3, 1), Pos(0, 1), Pos(0, 4), Pos(2, 2),
          Pos(4, 0), Pos(1, 0), Pos(1, 3), Pos(3, 5), Pos(3, 2), Pos(6, 2), Pos(9, 2), Pos(7, 0), Pos(5, 2), Pos(3, 4),
          Pos(5, 6), Pos(2, 6), Pos(4, 4), Pos(7, 4), Pos(7, 1), Pos(4, 1), Pos(2, 3), Pos(5, 3), Pos(7, 5), Pos(9, 3),
          Pos(9, 0), Pos(6, 0), Pos(6, 3), Pos(8, 5), Pos(8, 2), Pos(6, 4), Pos(4, 2), Pos(4, 5), Pos(6, 7), Pos(9, 7),
          Pos(9, 4), Pos(7, 2), Pos(5, 4), Pos(2, 4), Pos(2, 7), Pos(5, 7), Pos(7, 9), Pos(4, 9), Pos(4, 6), Pos(7, 6)
        ))
      )

    forAll(results) { (pos: Pos, result: List[Pos]) =>
      ChessTour.solve(pos) should equal(result)
    }
  }

  property("ChessTour should be computed a tour for a given starting position of size (N*N) for N x N board") {
    val results =
      Table(
        ("pos", "result"),
        (Pos(4, 3), 100)
      )

    forAll(results) { (pos: Pos, result: Int) =>
      ChessTour.solve(pos).size should equal(result)
    }
  }
}
