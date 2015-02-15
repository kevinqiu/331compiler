package lex

import collection.mutable.ListBuffer
import scala.util.{Try, Success, Failure}

object LexicalDriver {
  def main(args: Array[String]) {
    val file = "/home/kevin/git/331compiler/src/main/resources/lextest.dat"
    var stream = new CharStream(file)
    val la = new LexicalAnalyzer(stream)
    var tokens = new ListBuffer[Try[Token]]
    while (!la.end) {
      val t = la.getToken()
      tokens += t
      println(t)
    }
  }
}
