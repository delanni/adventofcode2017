package com.monkeybusiness.adventofcode2017

object Challenge1 extends ChallengeSolver {
  val day = 1

  override def solveFirst(input: String): Option[String] = {
    val endPrependedString = input.last + input.substring(0, input.length - 1)

    val result = endPrependedString.zip(input).map({
      case (a,b) if a == b =>
        a.toString.toInt
      case (a,b) =>
        0
    }).sum

    Some(result.toString)
  }

  override def solveSecond(input: String): Option[String] = {
    val halfLength = input.length / 2
    val endPrependedString = input.substring(halfLength) + input.substring(0, halfLength)

    val result = endPrependedString.zip(input).map({
      case (a,b) if a == b => a.toString.toInt
      case (a,b) => 0
    }).sum

    Some(result.toString)
  }
}
