package com.iamsmkr.chesstour.heuristic

import com.iamsmkr.chesstour.heuristic.ChessTour3X3.Pos
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ChessTour3X3Spec extends AnyPropSpec with should.Matchers with ScalaCheckPropertyChecks {
  property("ChessTour3X3 should be computed a tour for a given starting position") {
    val results =
      Table(
        ("pos", "result"),
        (Pos(0, 0), List(Pos(0, 0), Pos(0, 1), Pos(0, 2), Pos(1, 2), Pos(2, 2), Pos(1, 1), Pos(1, 0), Pos(2, 0), Pos(2, 1))),
        (Pos(1, 1), List(Pos(1, 1), Pos(0, 0), Pos(0, 1), Pos(0, 2), Pos(1, 2), Pos(2, 2), Pos(2, 1), Pos(1, 0), Pos(2, 0))),
        (Pos(2, 2), List(Pos(2, 2), Pos(1, 2), Pos(0, 2), Pos(0, 1), Pos(0, 0), Pos(1, 0), Pos(1, 1), Pos(2, 0), Pos(2, 1))),
        (Pos(2, 0), List(Pos(2, 0), Pos(1, 0), Pos(0, 0), Pos(0, 1), Pos(0, 2), Pos(1, 1), Pos(1, 2), Pos(2, 1), Pos(2, 2)))
      )

    forAll(results) { (pos: Pos, result: List[Pos]) =>
      ChessTour3X3.solve(pos) should equal(result)
    }
  }

  property("ChessTour3X3 should be computed a tour for a given starting position of size (N*N) for N x N board") {
    val results =
      Table(
        ("pos", "result"),
        (Pos(0, 0), 9),
        (Pos(1, 1), 9),
        (Pos(2, 2), 9),
        (Pos(2, 0), 9)
      )

    forAll(results) { (pos: Pos, result: Int) =>
      ChessTour3X3.solve(pos).size should equal(result)
    }
  }
}
