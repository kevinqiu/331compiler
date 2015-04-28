package symbol

import lex._
import parser._
import compiler._

import collection.mutable.HashMap

abstract class SymbolTableEntry{ val name: String }

trait DataEntry {
  val dataType: String
}

trait Address {
  val address: Int
}

trait Params {
  val numberOfParameters: Int
  val parameterInfo: List[SymbolTableEntry with DataEntry]
}


case class ArrayEntry(name: String, address: Int, dataType: String, upperBound: Int, lowerBound: Int) extends SymbolTableEntry with DataEntry with Address

case class ConstantEntry(name: String, dataType: String) extends SymbolTableEntry with DataEntry

case class FunctionEntry(name: String, numberOfParameters: Int, parameterInfo: List[SymbolTableEntry with DataEntry], result: String, dataType: String) extends SymbolTableEntry with DataEntry with Params

case class ProcedureEntry(name: String, numberOfParameters: Int, parameterInfo: List[SymbolTableEntry with DataEntry]) extends SymbolTableEntry with Params

case class VariableEntry(name: String, address: Int, dataType: String) extends SymbolTableEntry with DataEntry with Address


class SymbolTable extends HashMap[String, SymbolTableEntry] {

  def lookup(name: String): Option[SymbolTableEntry] = {
    this.get(name)
  }

  def insert(entry: SymbolTableEntry): Option[SymbolTableEntry] = {
    this.put(entry.name, entry)
  }

  def dumpTable(): Unit = {
    this.foreach(e => println(e._2))
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
        case t: IDENTIFIER => globalTable.insert(VariableEntry(t.value, 0, "IDENTIFIER"))
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

