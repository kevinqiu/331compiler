package compiler
import lex._
import parser._
import symbol._

object Compiler {
  def main(args: Array[String]) {
    if (args.length == 0) {
      println("No input file specified")
    } else {
      ParseDriver.run(args(0).toString)
    }
  }
}
