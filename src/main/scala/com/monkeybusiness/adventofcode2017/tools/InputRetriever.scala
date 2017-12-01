package com.monkeybusiness.adventofcode2017.tools

object InputRetriever {

  import scalaj.http._

  val sessionCookie: String = scala.io.Source.fromFile("cookie.ini").mkString

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
    val response: HttpResponse[String] = Http(s"http://adventofcode.com/2017/day/$day/answer")
      .cookie("session", sessionCookie)
      .postForm(postData)
      .asString
    if (response.code == 200)
      Some(
        if (response.body.contains("That's the right answer!")) true
        else false
      )
    else
      None

  }
}
