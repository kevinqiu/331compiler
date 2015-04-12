package symbol

import lex._
import parser._
import compiler._

import collection.mutable.HashMap

abstract class SymbolTableEntry{ val name: String }

case class ArrayEntry(name: String, address: Int, dataType: String, upperBound: Int, lowerBound: Int) extends SymbolTableEntry

case class ConstantEntry(name: String, dataType: String) extends SymbolTableEntry

case class FunctionEntry(name: String, numberOfParameters: Int, parameterInfo: Int, result: Any) extends SymbolTableEntry

case class ProcedureEntry(name: String, numberOfParameters: Int, parameterInfo: Int) extends SymbolTableEntry

case class VariableEntry(name: String, dataType: String) extends SymbolTableEntry


class SymbolTable {

  var table = new HashMap[String, SymbolTableEntry]()

  def lookup(name: String): Option[SymbolTableEntry] = {
    table.get(name)
  }

  def insert(entry: SymbolTableEntry): Option[SymbolTableEntry] = {
    table.put(entry.name, entry)
  }

  def dumpTable(): Unit = {
    table.foreach(e => println(e._2))
  }

}

object SymbolTableDriver {
  def run(file: String) = {
    val keywordTable = new SymbolTable
    val globalTable = new SymbolTable
    val constantTable = new SymbolTable

    var stream = new CharStream(file)
    val la = new LexicalAnalyzer(stream)
    val tokens = la.toList
    tokens.filter(_.isFailure).foreach(f => println("Error encountered: "+f.toString))
    tokens.foreach(tryToken =>
      tryToken.map(x => x match {
        case t: INTCONSTANT => constantTable.insert(ConstantEntry(t.value, "INTCONSTANT"))
        case t: REALCONSTANT => constantTable.insert(ConstantEntry(t.value, "REALCONSTANT"))
        case t: IDENTIFIER => globalTable.insert(VariableEntry(t.value, "IDENTIFIER"))
        case _ =>
      }))
    println("keywordTable: ")
    keywordTable.dumpTable()
    println("globalTable: ")
    globalTable.dumpTable()
    println("constantTable: ")
    constantTable.dumpTable()
  }
}

