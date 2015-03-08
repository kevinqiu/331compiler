package lex

import collection.mutable.ListBuffer
import scala.util.{Try, Success, Failure}
import java.io._
import parser._

object LexicalDriver {
  /*def main(args: Array[String]) {
    val file = "/home/kevin/git/331compiler/test.dat"
    val writeFile = new File("/home/kevin/git/331compiler/output")
    val bw = new BufferedWriter(new FileWriter(writeFile))
    var stream = new CharStream(file)
    val la = new LexicalAnalyzer(stream)
    var tokens = new ListBuffer[Try[Token]]
    while (!la.end) {
      val t = la.getToken()
      tokens += t
      println(t)
      bw.write(t.toString+"\n")
    }
    bw.close()
  }*/

  def main(args: Array[String]) = {
    val file = "/home/kevin/git/331compiler/parsetest.dat"
    var stream = new CharStream(file)
    val la = new LexicalAnalyzer(stream)
    val parser = new Parser(la)
    parser.parse()
  }
}
