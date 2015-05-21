package compiler
import lex._
import parser._
import symbol._
import java.io._

object Compiler {
  def main(args: Array[String]) {
    if (args.length == 0) {
      println("No input file specified")
    } else {
      val tvi = ParseDriver.run(args(0).toString)
      val writeFile = new File("output.tvi")
      val bw = new BufferedWriter(new FileWriter(writeFile))
      bw.write(tvi)
      bw.close()
      println("TVI written to output.tvi")
    }
  }
}
