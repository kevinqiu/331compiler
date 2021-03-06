package lex

import collection.mutable.ListBuffer
import scala.util.{Try, Success, Failure}
import java.io._
import compiler._

object LexicalDriver {
  def run(file: String = "/home/kevin/git/331compiler/lextest.dat") = {
    val writeFile = new File("/home/kevin/git/331compiler/output")
    val bw = new BufferedWriter(new FileWriter(writeFile))
    var stream = new CharStream(file)
    val la = new LexicalAnalyzer(stream)
    la.foreach(t => {
      println(t)
      bw.write(t.toString+"\n")
    })
    bw.close()
  }
}
