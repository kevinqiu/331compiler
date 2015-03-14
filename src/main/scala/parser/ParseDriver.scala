package parser
import lex._

object ParseDriver {
  def run(file: String = "/home/kevin/git/331compiler/parsetest.dat") = {
    var stream = new CharStream(file)
    val la = new LexicalAnalyzer(stream)
    val parser = new Parser(la)
    val failed = parser.parse().filter(_.isFailure)
    if (failed.isEmpty) {
      println("Parse Successful")
    } else {
      println("Errors encountered")
      failed.foreach(println(_))
    }
  }
}
