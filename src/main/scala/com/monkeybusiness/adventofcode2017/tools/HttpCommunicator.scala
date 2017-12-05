package com.monkeybusiness.adventofcode2017.tools

import scala.io.Source

object HttpCommunicator {

  import scalaj.http._

  val sessionCookie: String = Source.fromFile("cookie.ini").mkString

  def getInput(day: Int): Option[String] = {
    val response: HttpResponse[String] = Http(s"http://adventofcode.com/2017/day/$day/input")
      .cookie("session", sessionCookie)
      .asString
    if (response.code == 200)
      Some(response.body.trim)
    else
      None
  }

  def sendAnswer(day: Int, level: Int, answer: String): Option[Boolean] = {
    val postData = Seq(("level", s"$level"), ("answer", answer))

    println(s"Sending solution $day/$level: $answer 📨")
    val response: HttpResponse[String] = Http(s"http://adventofcode.com/2017/day/$day/answer")
      .cookie("session", sessionCookie)
      .postForm(postData)
      .asString

    if (response.code == 200)
      if (response.body.contains("left to wait.")) None
      else if (response.body.contains("That's the right answer!")) Some(true)
      else if (response.body.contains("right level")) None
      else Some(false)
    else
      None

  }
}
