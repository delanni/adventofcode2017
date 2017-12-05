package com.monkeybusiness.adventofcode2017

object Challenge5 extends ChallengeSolver {
  //  import scala.collection.immutable.Seq
  val day = 5

  // (A)dvential (C)omputational (I)nstruction (D)eterminator
  sealed trait ACID {
    def stepOne: ACID

    def isTerminated: Boolean

    def instructionCounter: Int

    def instructionPointer: Int
  }

  // TODO: Modularize, to allow for mutable updating
  //  case class ACIDModule(
  //                         updaterModule: (Seq[Int], Int, Int) => (Seq[Int], Int, Int), instructions: Seq[Int],
  //                         instructionCounter: Int,
  //                         instructionPointer: Int,
  //                         isTerminated: Boolean) {
  //    def stepOne: ACIDModule = {
  //      if (isTerminated || instructionPointer < 0 || instructionPointer >= instructions.length) {
  //        ACIDModule(updaterModule, instructions, instructionCounter, instructionPointer, isTerminated = true)
  //      } else {
  //        val (updatedInstructions, updatedInstructionCounter, updatedInstructionPointer) = updaterModule(instructions, instructionCounter, instructionPointer)
  //        ACIDModule(
  //          updaterModule,
  //          updatedInstructions,
  //          updatedInstructionCounter,
  //          updatedInstructionPointer,
  //          isTerminated = false
  //        )
  //      }
  //    }
  //  }

  case class ACID1(instructions: Seq[Int], instructionCounter: Int, instructionPointer: Int, isTerminated: Boolean) extends ACID {
    def stepOne: ACID1 = {
      if (isTerminated || instructionPointer < 0 || instructionPointer >= instructions.length) {
        ACID1(instructions, instructionCounter, instructionPointer, isTerminated = true)
      } else {
        val nextInstruction: Int = instructions(instructionPointer)
        ACID1(
          instructions.updated(instructionPointer, nextInstruction + 1),
          instructionCounter + 1,
          instructionPointer + nextInstruction,
          isTerminated = false
        )
      }
    }
  }

  case class ACID2(instructions: Seq[Int], instructionCounter: Int, instructionPointer: Int, isTerminated: Boolean) extends ACID {
    def stepOne: ACID2 = {
      if (isTerminated || instructionPointer < 0 || instructionPointer >= instructions.length) {
        ACID2(instructions, instructionCounter, instructionPointer, isTerminated = true)
      } else {
        val nextInstruction: Int = instructions(instructionPointer)
        ACID2(
          instructions.updated(instructionPointer, if (nextInstruction >= 3) nextInstruction - 1 else nextInstruction + 1),
          instructionCounter + 1,
          instructionPointer + nextInstruction,
          isTerminated = false
        )
      }
    }
  }

  case class ACID2_mutable(instructions: Array[Int], var instructionCounter: Int, var instructionPointer: Int, var isTerminated: Boolean) extends ACID {
    def stepOne: ACID2_mutable = {
      if (isTerminated || instructionPointer < 0 || instructionPointer >= instructions.length) {
        this.isTerminated = true
        this
      } else {
        val nextInstruction: Int = instructions(instructionPointer)
        this.instructions.update(instructionPointer, if (nextInstruction >= 3) nextInstruction - 1 else nextInstruction + 1)
        this.instructionCounter = this.instructionCounter + 1
        this.instructionPointer = instructionPointer + nextInstruction
        this
      }
    }
  }

  def runMutableUntilTermination(machine: ACID2_mutable): ACID2_mutable = {
    while (!machine.isTerminated) {
      machine.stepOne
    }
    machine
  }

  def runUntilTermination(machine: ACID): ACID = {
    if (machine.isTerminated) {
      machine
    } else {
      runUntilTermination(machine.stepOne)
    }
  }

  // NOTE: Slow A.F.
  override def solveFirst(input: String): Option[String] = {
    val instructions = input.split("\\n").map(row => row.trim.toInt)

    val acid = ACID1(Seq(instructions: _*), 0, 0, isTerminated = false)

    val terminatedAcid = runUntilTermination(acid)

    Some(terminatedAcid.instructionCounter.toString)
  }

  // NOTE: Slow A.F.
  //  override def solveSecond(input: String): Option[String] = {
  //    val instructions = input.split("\\n").map(row => row.trim.toInt)
  //
  //    val acid = ACID2(Seq(instructions: _*), 0, 0, isTerminated = false)
  //
  //    val terminatedAcid = runUntilTermination(acid)
  //
  //    Some(terminatedAcid.instructionCounter.toString)
  //  }

  override def solveSecond(input: String): Option[String] = {
    val instructions = input.split("\\n").map(row => row.trim.toInt)

    val acid = ACID2_mutable(instructions, 0, 0, isTerminated = false)

    val terminatedAcid = runMutableUntilTermination(acid)

    Some(terminatedAcid.instructionCounter.toString)
  }
}

