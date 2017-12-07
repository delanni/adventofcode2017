package com.monkeybusiness.adventofcode2017

import scala.collection.mutable

object Challenge7 extends ChallengeSolver {
  val day = 7

  object Node {
    val allNodes: mutable.HashMap[String, Node] = mutable.HashMap.empty

  }

  case class Node(name: String, value: Int, children: Seq[String]) {
    def childNodes: Seq[Node] = children.flatMap(x => {
      Node.allNodes.get(x) match {
        case Some(child) => Seq(child)
        case None => Seq()
      }
    })

    def weight: Int = value + childNodes.map(_.weight).sum

    def isBalanced(): Boolean = {
      if (this.children.isEmpty) {
        true
      }
      else {
        val weightPerChild = (this.weight - this.value) / children.length
        childNodes.forall(_.weight == weightPerChild)
      }
    }
  }


  def nodeListFromRows(rows: Array[String]): Array[Node] = {
    val leftNameMatcher = """([a-z]+) \((\d+)\).*""".r.unanchored

    rows.map(row => {
      val n = row.split("->") match {
        case Array(leftNameMatcher(leftName, count), rightList) =>
          Node(leftName.trim, count.toInt, rightList.split(",").map(_.trim))
        case Array(leftNameMatcher(leftName, count)) =>
          Node(leftName.trim, count.toInt, Seq.empty)
        case _ => throw new Exception("well shit")
      }
      Node.allNodes.put(n.name, n)
      n
    })
  }

  override def solveFirst(input: String): Option[String] = {
    val rows = input.split("\\n")

    val nodes = nodeListFromRows(rows)

    val parentNames = nodes.map(x => x.name).toSet
    val childNames = nodes.flatMap(x => x.children).distinct.toSet

    val rootNames: Set[String] = parentNames -- childNames

    Some(rootNames.head)
  }


  def getOutstandingNode(childNodes: Seq[Node]): (Node, Int) =  {
    val groups = childNodes.groupBy(_.weight)
    val oddNode = groups.find(_._2.length == 1)
    val weights = childNodes.map(_.weight).distinct.toSet
    oddNode.map(node => {
      (node._2.head, (weights - node._2.head.weight).head)
    }) match {
      case Some(value) => value
      case None => throw new Exception("shit2")
    }
  }

  override def solveSecond(input: String): Option[String] = {
    val rows = input.split("\\n")

    Node.allNodes.clear()

    val nodes = nodeListFromRows(rows)

    val unbalancedNode = nodes.find(x => !x.isBalanced)

    unbalancedNode match {
      case Some(node) =>
        val (badNode, targetWeight) = getOutstandingNode(node.childNodes)
        val childWeight = badNode.weight - badNode.value
        Some((targetWeight - childWeight).toString)

      case None => None
    }
  }
}

