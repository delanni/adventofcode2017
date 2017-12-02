package com.monkeybusiness.adventofcode2017

object Challenge2 extends ChallengeSolver {
  val day = 2

  override def solveFirst(input: String): Option[String] = {
    val result = input.split("\\n").map(row => {
      val numRow = row.split("\\s+").map(_.trim.toInt)
      numRow.max - numRow.min
    }).sum

    Some(result.toString)
  }

  override def solveSecond(input: String): Option[String] = {
    val result = input.split("\\n").map(row => {
      val numRow = row.split("\\s+").map(_.trim.toInt)
      numRow.combinations(2).map({
        case Array(a, b) if a % b == 0 => a / b
        case Array(a, b) if b % a == 0 => b / a
        case _ => 0
      }).sum
    }).sum

    Some(result.toString)
  }
}
