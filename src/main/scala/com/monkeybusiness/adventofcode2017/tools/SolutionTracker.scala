package com.monkeybusiness.adventofcode2017.tools

import java.io.{File, PrintWriter}

import scala.io.Source

object SolutionTracker {

  val fileName = "solutions.txt"

  object Solution {
    val splitter = " 🙉 "

    def apply(line: String): Solution = {
      val segments = line.split(splitter)
      val day = segments(0).toInt
      val task = segments(1).toInt
      val answer = segments(2).trim
      Solution(day, task, answer)
    }
  }

  case class Solution(day: Int, task: Int, answer: String) {
    override def toString: String = s"$day${Solution.splitter}$task${Solution.splitter}$answer"
  }

  def countSolutionsForTheDay(day: Int): Int = {
    val solutionsForTheDay = readSolutions.filter(_.day == day)
    solutionsForTheDay.length
  }

  def readSolutions: Seq[Solution] = {
    Source.fromFile(fileName).getLines.map(Solution(_)).toSeq
  }

  def writeSolutions(solutions: Seq[Solution]): Unit = {
    val content = solutions.mkString("\n")

    val pw = new PrintWriter(new File(fileName))
    pw.write(content)
    pw.close()
  }

}