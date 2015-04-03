package symbol

import collection.mutable.HashMap

class SymbolTableEntry(name: String)

case class ArrayEntry(name: String, address: String, dataType: String, upperBound: String, lowerBound: String) extends SymbolTableEntry(name)

case class ConstantEntry(name: String, dataType: String) extends SymbolTableEntry(name)

case class FunctionEntry(name: String, numberOfParameters: Int, parameterInfo: Int, result: Any) extends SymbolTableEntry(name)

case class ProcedureEntry(name: String, numberOfParameters: Int, parameterInfo: Int) extends SymbolTableEntry(name)

case class VariableEntry(name: String, address: String, dataType: String) extends SymbolTableEntry(name)


class SymbolTable {

  var table = new HashMap[String, SymbolTableEntry]()

  def lookup(entry: SymbolTableEntry) = {
    table.getOrElseUpdate("", entry)
  }

  def insert(entry: SymbolTableEntry) = {
    table.put("", entry)
  }

  def dumbTable() = {
    println(table.toString)
  }

}

