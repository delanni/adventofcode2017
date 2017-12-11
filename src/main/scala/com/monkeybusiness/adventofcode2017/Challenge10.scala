package com.monkeybusiness.adventofcode2017

object Challenge10 extends ChallengeSolver {
  val day = 10

  case class CircularList[T](base: Iterable[T]) {
    def sublist(from: Int, items: Int): Iterable[T] = {
      if (from + items > base.size) {
        val takenAmount = base.size - from
        base.slice(from, base.size) ++ this.sublist(0, items - takenAmount)
      } else {
        base.slice(from, from + items)
      }
    }

    def update(from: Int, sublist: Iterable[T]): Iterable[T] = {
      val untouched = base.take(from)

      if (from + sublist.size > base.size) {
        val end = sublist.take(base.size - from)
        CircularList[T](untouched ++ end).update(0, sublist.drop(base.size - from))
      } else {
        val untouchedEnd = base.slice(from + sublist.size, base.size)
        untouched ++ sublist ++ untouchedEnd
      }
    }
  }

  class MagicHash(private val _array: Array[Int]) {
    private var currentPtr = 0
    private var skipLength = 0
    var circularList = CircularList(_array)

    def array: Array[Int] = circularList.base.toArray

    def flip(length: Int): MagicHash = {
      val sublist = circularList.sublist(currentPtr, length)
      this.circularList = CircularList(circularList.update(currentPtr, sublist.toArray.reverse))
      currentPtr = (currentPtr + length + skipLength) % _array.length
      skipLength += 1
      this
    }
  }

  override def solveFirst(input: String): Option[String] = {
    val lengths = input.split(",").map(_.trim.toInt)

    val magicHash = new MagicHash((0 to 255).toArray)

    for {l <- lengths} {
      magicHash.flip(l)
    }

    val result = magicHash.array

    Some((result(0) * result(1)).toString)
  }

  override def solveSecond(input: String): Option[String] = {
    val lengths: Array[Int] = input.trim.toCharArray.map(_.toInt) ++ Array(17, 31, 73, 47, 23)

    val magicHash = new MagicHash((0 to 255).toArray)

    for (round <- 1 to 64; l <- lengths) {
      magicHash.flip(l)
    }

    val array = magicHash.array

    val hash = (0 to 15)
      .map(hashIndex => array.slice(hashIndex * 16, (hashIndex + 1) * 16).reduce((a, b) => a ^ b))
      .map(_.toHexString)
      .map(x => if (x.length < 2) s"0$x" else x)
      .mkString("")

    Some(hash)
  }
}

