package com.monkeybusiness.adventofcode2017

import scala.annotation.tailrec

object Challenge6 extends ChallengeSolver {
  val day = 6

  def memoryAsKey(memory: Array[Int]): String = memory.mkString("_")


  def spreadElement(array: Array[Int], index: Int): Array[Int] = {
    val arrayClone = array.clone()
    val size = arrayClone.length

    val valueToSpread = arrayClone(index)
    arrayClone.update(index, 0)

    for {
      offset <- 1 to valueToSpread
    } {
      val updatedIndex = (index + offset) % size
      arrayClone.update(updatedIndex, arrayClone(updatedIndex) + 1)
    }
    arrayClone
  }

  def maxValueIndex(array: Array[Int]): Int = {
    val maxValue = array.max
    array.indexOf(maxValue)
  }

  @tailrec
  def spreadUntilEquilibrium(keysSeen: Map[String, Array[Int]], memory: Array[Int]): (Map[String, Array[Int]], Array[Int]) = {
    val nextMemoryState = spreadElement(memory, maxValueIndex(memory))
    val key = memoryAsKey(nextMemoryState)
    if (keysSeen.contains(key)) {
      (keysSeen, memory)
    } else {
      spreadUntilEquilibrium(keysSeen + ((key, nextMemoryState)), nextMemoryState)
    }
  }

  override def solveFirst(input: String): Option[String] = {
    val initialArray = input.split("\\s+").map(_.toInt)
    val key: String = memoryAsKey(initialArray)

    val (equilibriumKeys, _) = spreadUntilEquilibrium(Map((key, initialArray)), initialArray)

    Some(equilibriumKeys.size.toString)
  }


  override def solveSecond(input: String): Option[String] = {
    val initialArray = input.split("\\s+").map(_.toInt)
    val key: String = memoryAsKey(initialArray)

    val (_, equilibriumArray) = spreadUntilEquilibrium(Map((key, initialArray)), initialArray)

    val (infiniteLoopKeys, _) = spreadUntilEquilibrium(Map.empty, equilibriumArray)

    Some(infiniteLoopKeys.size.toString)
  }
}

