package com.monkeybusiness.adventofcode2017

object Challenge4 extends ChallengeSolver {
  val day = 4

  override def solveFirst(input: String): Option[String] = {
    val result = input.split("\\n").map(row => {
      val words = row.trim.split("\\s+")
      if (words.distinct.length == words.length) {
        1
      } else {
        0
      }
    }).sum

    Some(result.toString)
  }

  override def solveSecond(input: String): Option[String] = {
    val result = input.split("\\n").map(row => {
      val words = row.trim.split("\\s+").map(_.sorted)
      if (words.distinct.length == words.length) {
        1
      } else {
        0
      }
    }).sum

    Some(result.toString)
  }
}
