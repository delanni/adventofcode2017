package com.monkeybusiness.adventofcode2017

import com.monkeybusiness.adventofcode2017.tools.InputRetriever

object Challenge1 {
  def solveFirst(input: String): Int = {
    val endPrependedString = input.last + input.substring(0, input.length - 1)

    endPrependedString.zip(input).map({
      case (a,b) if a == b =>
        a.toString.toInt
      case (a,b) =>
        0
    }).sum
  }

  def solveSecond(input: String): Int = {
    val halfLength = input.length / 2
    val endPrependedString = input.substring(halfLength) + input.substring(0, halfLength)

    endPrependedString.zip(input).map({
      case (a,b) if a == b => a.toString.toInt
      case (a,b) => 0
    }).sum
  }

  def solve(): Unit ={
    InputRetriever.getInput(1) match {
      case Some(input) =>
        println("1/1: " + solveFirst(input))
        println("2/2: " + solveSecond(input))
        InputRetriever.sendAnswer(1, 2, solveSecond(input).toString)
      case None =>
        println("Nothing to solve")
    }

  }
}
