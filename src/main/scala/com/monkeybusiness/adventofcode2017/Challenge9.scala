package com.monkeybusiness.adventofcode2017

import scala.annotation.tailrec

object Challenge9 extends ChallengeSolver {
  val day = 9

  sealed trait State

  case class Read(depth: Int) extends State

  case class GarbageRead(depth: Int) extends State

  case object Finished extends State

  case class Block(depth: Int)

  // Byte Driven State Machine
  case class BDSM(state: State, pointer: Int, chars: Array[Char], openBlocks: List[Block], finishedBlocks: List[Block], ignoredCount: Int) {

    def step(): BDSM = {
      if (pointer >= chars.length) {
        this.copy(Finished)
      } else {
        val char = chars(pointer)

        state match {
          case Read(depth) =>
            char match {
              case '{' => BDSM(Read(depth + 1), pointer + 1, chars, Block(depth + 1) :: openBlocks, finishedBlocks, ignoredCount)
              case '}' => BDSM(Read(depth - 1), pointer + 1, chars, openBlocks.tail, finishedBlocks :+ openBlocks.head, ignoredCount)
              case '!' => next.next
              case '<' => BDSM(GarbageRead(depth), pointer + 1, chars, openBlocks, finishedBlocks, ignoredCount)
              case '>' => next
              case a@_ => next
            }
          case GarbageRead(depth) =>
            char match {
              case '!' => next.next
              case '>' => BDSM(Read(depth), pointer + 1, chars, openBlocks, finishedBlocks, ignoredCount)
              case a@_ => BDSM(state, pointer + 1, chars, openBlocks, finishedBlocks, ignoredCount + 1)
            }
          case Finished =>
            throw new Error("Machine progressed in finished state")
        }
      }
    }

    def next: BDSM = {
      BDSM(state, pointer + 1, chars, openBlocks, finishedBlocks, ignoredCount)
    }

    @tailrec
    final def runUntilEnd(): BDSM = {
      if (this.state == Finished) {
        this
      } else {
        val nextState = this.step()
        nextState.runUntilEnd()
      }
    }
  }

  override def solveFirst(input: String): Option[String] = {
    val characters = input.trim.toCharArray

    val machine = BDSM(Read(0), 0, characters, List.empty, List.empty, 0)

    val finished = machine.runUntilEnd()

    val totalValue = finished.finishedBlocks.map(_.depth).sum

    Some(totalValue.toString)
  }

  override def solveSecond(input: String): Option[String] = {
    val characters = input.trim.toCharArray

    val machine = BDSM(Read(0), 0, characters, List.empty, List.empty, 0)

    val finished = machine.runUntilEnd()

    Some(finished.ignoredCount.toString)
  }
}

