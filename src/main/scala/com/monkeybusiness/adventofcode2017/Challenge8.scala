package com.monkeybusiness.adventofcode2017

import scala.collection.mutable

object Challenge8 extends ChallengeSolver {
  val day = 8

  var systemMax = 0

  case class Operation(operator: String, value: Int) {
    def executeOn(registerValue: Int): Int = {
      val newValue = operator match {
        case "inc" => registerValue + value
        case "dec" => registerValue - value
      }
      systemMax = Math.max(systemMax, newValue)
      newValue
    }
  }

  case class Check(testRegister: String, operator: String, testValue: Int) {
    def execute(register: Register): Boolean = {
      val registerValue = register.getOrElse(testRegister, 0)
      operator match {
        case ">" => registerValue > testValue
        case "<" => registerValue < testValue
        case ">=" => registerValue >= testValue
        case "<=" => registerValue <= testValue
        case "!=" => registerValue != testValue
        case "==" => registerValue == testValue
        case _ => throw new Error("Missing operator")
      }
    }
  }

  type Register = Map[String, Int]

  case class OperationStep(registerName: String, operation: Operation, predicate: Check) {
    def execute(register: Register): Register = {
      if (predicate.execute(register)) {
        register.get(registerName) match {
          case Some(registerValue) => register.updated(registerName, operation.executeOn(registerValue))
          case None => register.updated(registerName, operation.executeOn(0))
        }
      } else {
        register
      }
    }
  }

  override def solveFirst(input: String): Option[String] = {
    val operationSteps = input.trim.split("\\n").map(x => {
      val tokens: Array[String] = x.trim.split(" ")
      tokens match {
        case Array(_targetRegister, _operation, _changeAmount, _if, _testRegister, _testOperator, _testValue) =>
          OperationStep(_targetRegister, Operation(_operation, _changeAmount.toInt), Check(_testRegister, _testOperator, _testValue.toInt))
        case _ => throw new Exception("Input line does not conform expected format: " + x)
      }
    })

    val finalRegisterState = operationSteps.foldLeft(Map[String, Int]())((register, operation) => operation.execute(register))

    Some(finalRegisterState.maxBy(_._2)._2.toString)
  }

  override def solveSecond(input: String): Option[String] = {
    val operationSteps = input.trim.split("\\n").map(x => {
      val tokens: Array[String] = x.trim.split(" ")
      tokens match {
        case Array(_targetRegister, _operation, _changeAmount, _if, _testRegister, _testOperator, _testValue) =>
          OperationStep(_targetRegister, Operation(_operation, _changeAmount.toInt), Check(_testRegister, _testOperator, _testValue.toInt))
        case _ => throw new Exception("Input line does not conform expected format: " + x)
      }
    })

    operationSteps.foldLeft(Map[String, Int]())((register, operation) => operation.execute(register))

    Some(systemMax.toString)
  }
}

