package com.monkeybusiness.adventofcode2017

import scala.collection.immutable.HashMap

object Challenge3 extends ChallengeSolver {
  val day = 3

  override def solveFirst(input: String): Option[String] = {
    val targetNumber = input.trim.toInt

    val nThLayer = getLayerNumber(targetNumber)

    val lastSteps = getLastSteps(targetNumber, nThLayer)

    println(s"Layer: $nThLayer , steps: $lastSteps")

    Some((nThLayer + lastSteps).toString)
  }

  override def solveSecond(input: String): Option[String] = {
    val inputNumber = input.trim.toInt

    val firstCell = CellType(0, 0, 1, 1)
    val (correctCell, _) = fillNextNumber(inputNumber, firstCell, HashMap.empty + ((firstCell.key, firstCell)))

    Some(correctCell.value.toString)
  }

  def getLayerNumber(targetNumber: Int): Int = {
    Math.ceil(Math.ceil(Math.sqrt(targetNumber) + 1) / 2 - 1).toInt
  }

  def getIndicesByOrdinal(ordinal: Int): (Int, Int) = {
    val layerNumber = Math.ceil((Math.sqrt(ordinal) - 1) / 2).toInt
    val nThEvenNumber = 2 * layerNumber + 1
    val layerEnd = nThEvenNumber * nThEvenNumber
    val nThOddNumber = nThEvenNumber - 1
    ordinal match {
      case _ if ordinal >= layerEnd - nThOddNumber => (layerNumber - (layerEnd - ordinal), - layerNumber)
      case _ if ordinal >= layerEnd - nThOddNumber * 2 => (- layerNumber, - layerNumber + (layerEnd - ordinal - nThOddNumber) )
      case _ if ordinal >= layerEnd - nThOddNumber * 3 => (- layerNumber + (layerEnd - ordinal - nThOddNumber * 2), layerNumber)
      case _ => (layerNumber, layerNumber - (layerEnd - ordinal - nThOddNumber * 3) )
    }
  }

  def getLastSteps(targetNumber: Int, nThLayer: Int): Int = {
    if (nThLayer == 0) 0
    else {
      val nThOddNumber = nThLayer * 2 + 1
      val distanceFromMax = Math.min(Math.abs(nThOddNumber * nThOddNumber - targetNumber), Math.abs((nThOddNumber - 2) * (nThOddNumber - 2) - targetNumber))
      Math.abs(distanceFromMax % (nThOddNumber - 1) - nThLayer)
    }
  }

  object CellType {
    def asKey(x: Int, y: Int) = s"($x:$y)"
  }

  case class CellType(x: Int, y: Int, ordinal: Int, value: Int) {
    def key: String = CellType.asKey(this.x, this.y)

    override def toString: String = s"($x, $y)/#$ordinal = $value"
  }

  type CellMap = HashMap[String, CellType]

  def getNeighbours(x: Int, y: Int, memory: CellMap): Seq[CellType] = {
    Seq(CellType.asKey(x, y),
      CellType.asKey(x + 1, y),
      CellType.asKey(x - 1, y),
      CellType.asKey(x, y + 1),
      CellType.asKey(x, y - 1),
      CellType.asKey(x + 1, y + 1),
      CellType.asKey(x - 1, y - 1),
      CellType.asKey(x + 1, y - 1),
      CellType.asKey(x - 1, y + 1)).map(memory.get).flatMap({
      case Some(cell) => Seq(cell)
      case None => Seq()
    })
  }

  def fillNextNumber(target: Int, currentCell: CellType, memory: CellMap): (CellType, CellMap) = {
    if (currentCell.value > target) {
      (currentCell, memory)
    } else {
      val nextOrdinal = currentCell.ordinal + 1
      val (nextX, nextY) = getIndicesByOrdinal(nextOrdinal)

      val neighboursValue = getNeighbours(nextX, nextY, memory).map(_.value).sum

      val nextCell = CellType(nextX, nextY, nextOrdinal, neighboursValue)

      fillNextNumber(target, nextCell, memory + ((nextCell.key, nextCell)))
    }
  }
}
