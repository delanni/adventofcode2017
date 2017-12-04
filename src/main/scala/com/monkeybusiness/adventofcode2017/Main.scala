package com.monkeybusiness.adventofcode2017

import com.monkeybusiness.adventofcode2017
import com.monkeybusiness.adventofcode2017.tools.SolutionTracker.Solution
import com.monkeybusiness.adventofcode2017.tools.{HttpCommunicator, SolutionTracker}

object Main {

  def tryUpload(solution: Solution): Boolean = {
    HttpCommunicator.sendAnswer(solution.day, solution.task, solution.answer) match {
      case Some(true) =>
        println("Answer accepted! ✅")
        true
      case Some(false) =>
        println("Answer was incorrect! ❌")
        false
      case None =>
        println("Can't upload - 🚀")
        false
    }
  }

  def main(args: Array[String]): Unit = {

    val implementedSolutions = Seq(Challenge1, Challenge2, Challenge3, Challenge4)

    val solutions = SolutionTracker.readSolutions

    println("Current solutions:")
    println(solutions.mkString("\n"))

    val newSolutions = implementedSolutions.flatMap(challenge => {
      val solutionsCount = SolutionTracker.countSolutionsForTheDay(challenge.day)
      if (solutionsCount < 1) {
        println(s"Solving ${challenge.day}... ✍️ ✍️ ✍️")
        val solutions = challenge.solve()

        println(s"Solutions: ")
        println(s"${solutions._1.map(_.toString)} ")
        println(s"${solutions._2.map(_.toString)} ")

        Seq(solutions._1, solutions._2)
      } else if (solutionsCount < 2) {
        println(s"Solving ${challenge.day}/2... ✍️")
        val solutions = challenge.solve()

        println(s"Solutions: ")
        println(s"${solutions._2.map(_.toString)} ")

        Seq(solutions._2)
      } else {
        Seq()
      }
    }).filter(_.isDefined).map(_.get)

    val uploadedSolutions = newSolutions.filter(tryUpload)

    SolutionTracker.writeSolutions(solutions ++ uploadedSolutions)
  }
}
