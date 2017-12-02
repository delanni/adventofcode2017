package com.monkeybusiness.adventofcode2017

import com.monkeybusiness.adventofcode2017.tools.HttpCommunicator
import com.monkeybusiness.adventofcode2017.tools.SolutionTracker.Solution

trait ChallengeSolver {
  val day: Int

  def solveFirst(input: String): Option[String]

  def solveSecond(input: String): Option[String]

  def getInput: Option[String] = {
    HttpCommunicator.getInput(day)
  }

  def solve(): (Option[Solution], Option[Solution]) = {
    getInput match {
      case Some(input) =>
        val firstSolution = solveFirst(input)
        println(s"Solved $day / 1: ${firstSolution.map(_.toString)} 🐒")
        val secondSolution = solveSecond(input)
        println(s"Solved $day / 2: ${firstSolution.map(_.toString)} 🐒🐒")

        (firstSolution.map(Solution(day, 1, _)), secondSolution.map(Solution(day, 2, _)))
      case None =>
        (None, None)
    }
  }
}