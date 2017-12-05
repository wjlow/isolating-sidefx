package demo

import scalaj.http.Http

object Main {

  type RespBody = String

  sealed trait ProgramAction

  case class GetFromUrlAction(url: String) extends ProgramAction

  case class TransformAction(getAction: GetFromUrlAction, transformation: RespBody => RespBody) extends ProgramAction

  def main(args: Array[String]): Unit = {
    val url = "https://jsonplaceholder.typicode.com/posts/1"
    val transformAction = service(url)
    val resp = interpret(transformAction)
    println(resp)
  }

  def service(url: String): TransformAction = {
    TransformAction(
      GetFromUrlAction(url),
      (respBody: RespBody) => transform(respBody)
    )
  }

  def transform(str: String): String = {
    s"We received this HTTP response: $str"
  }

  def interpret(programAction: ProgramAction): RespBody =
    programAction match {
      case GetFromUrlAction(url) => Http(url).asString.body
      case TransformAction(getAction, f) => f(interpret(getAction))
    }

}