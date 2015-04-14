package semanticActions

import compiler._
import symbol._

import util.{Try, Success, Failure}
import collection.mutable.{Stack, ListBuffer}

class SemanticStack[A] extends Stack[A] {
  def popString(): String = {
    val element = this.pop()
    element match {
      case t: Token => t.value
      case s: String => s
      case _ => ""
    }
  }
}

abstract class ETYPE
case class ARITHMETIC extends ETYPE
case class RELATIONAL extends ETYPE

case class Quadruple(opCode: String, arg1: String, arg2: String, result: String)

class SemanticActions {
  var semanticStack: SemanticStack[Any] = new SemanticStack
  var globalTable = new SymbolTable
  var constantTable = new SymbolTable
  var localTable = new SymbolTable
  var quadruples = new ListBuffer[Quadruple]()
  var errors = new ListBuffer[CompilerError]
  var insert = false
  var isArray = false
  var global = false
  var globalMemory = 0
  var localMemory = 0
  var globalStore = 0
  var localStore = 0
  var nextQuad = 0
  var currentFunction = FunctionEntry("", 0, 0, "")
  var tmp = 0

  def action3(token: Token) = {
    val dataType = semanticStack.popString()

    if (isArray) {

      val upperBound = semanticStack.popString().toInt
      val lowerBound = semanticStack.popString().toInt
      val mSize = upperBound - lowerBound + 1

      while (semanticStack.head.isInstanceOf[IDENTIFIER]) {
        val id = semanticStack.popString()
        if (global) {
          globalTable.insert(ArrayEntry(id, globalMemory, dataType, upperBound, lowerBound))
          globalMemory = globalMemory + mSize
        } else {
          globalTable.insert(ArrayEntry(id, localMemory, dataType, upperBound, lowerBound))
          localMemory = localMemory + mSize
        }
      }

    } else {

      while (semanticStack.head.isInstanceOf[IDENTIFIER]) {
        val id = semanticStack.popString()
        if (global) {
          globalTable.insert(VariableEntry(id, globalMemory, dataType))
          globalMemory = globalMemory + 1
        } else {
          localTable.insert(VariableEntry(id, localMemory, dataType))
          localMemory = localMemory + 1
        }
      }

    }

    isArray = false
  }

  def action9(token: Token) = {
    while (semanticStack.head.isInstanceOf[IDENTIFIER]) {
      val id = semanticStack.popString()
      //picked abritary 0 for address entry, not sure if correct, revist later
      globalTable.insert(VariableEntry(id, 0, "restricted"))
    }
    insert = false
    gen("CODE")
    gen("call", "main", 0)
    gen("exit")
  }

  def action30(token: Token) = {
    val id = token.value
    val result = symbolTable().lookup(id)
    result.map((res) => {
      semanticStack.push(res)
      semanticStack.push(ARITHMETIC)
    })
    if (result.isEmpty) errors += UndeclaredVariable("Identifier " + id +" used before declaration")
  }

  def action31(token: Token) = {
    println(semanticStack)
    val eType = semanticStack.pop()/*
    val id1 = semanticStack.pop()
    val offset = semanticStack.pop()
    val id2 = semanticStack.pop()*/
  }

  //not complete
  def action34() = {
    if (semanticStack.head.isInstanceOf[FunctionEntry]) {
      //action52()
    } else {
      semanticStack.push(null)
    }
  }

  def action42(token: Token) = {
    val eType = semanticStack.pop()
    if (token == ADDOP(3, "or")) {
      if (eType != RELATIONAL) {
        errors += GenericSemanticError("Error at "+ token)
      }
      val eFalse = semanticStack.popString()
      println("action42: popping eFalse", eFalse)
      backPatch(eFalse.toInt, nextQuad)
    } else if (eType == ARITHMETIC) {

    }
    semanticStack.push(token)
  }

  //incomplete
  def action44(token: Token) = {
    val eType = semanticStack.pop()
    semanticStack.push(token)
  }

  def action46(token: Token) = {
    val id = token.value
    if (token.isInstanceOf[IDENTIFIER]) {
      val result = symbolTable().lookup(id)
      result.map((res) => {
        semanticStack.push(res)
      })
      if (result.isEmpty) errors += UndeclaredVariable("Identifier " + id +" used before declaration")
    } else {
      token match {
        case t: CONSTANT => {
          val result = constantTable.lookup(id)
          result match {
            case Some(res) => semanticStack.push(res)
            case None => constantTable.insert(ConstantEntry(id, t.getType))
          }
        }
        case _ =>
      }
    }
    semanticStack.push(ARITHMETIC)
  }

  def action48() = {
    if (semanticStack.head != null) {
      semanticStack.head match {
        case i: Integer => {
          val offset = semanticStack.pop()
          val eType = semanticStack.pop()
          val id = semanticStack.pop()
          id match {
            case entry: SymbolTableEntry with DataEntry => {
              val tmpEntry = create(tmp.toString, entry.dataType)
              tmp = tmp + 1
              gen("load", entry.name, offset, tmpEntry)
              semanticStack.push(tmpEntry)
              semanticStack.push(ARITHMETIC)
            }
          }
        }
        case _ => errors += GenericSemanticError("")
      }
    } else {
      semanticStack.pop()
    }
  }


  //not finished
/*  def action53(token: Token) = {
    if (semanticStack.head.isInstanceOf[FunctionEntry]){

    }
  }

  def action52() = {

  }*/

  def action55() = {
    backPatch(globalStore, globalMemory)
    gen("free", globalMemory)
    gen("PROCEND")
  }

  def action56() = {
    gen("PROCBEGIN", "main")
    globalStore = nextQuad
    gen("alloc", "")
  }

  def execute(action: SemanticAction, token: Token) = {
    println(action)
    action match {
      case Action1 => insert = true
      case Action2 => insert = false
      case Action3 => action3(token)
      case Action4 => semanticStack.push(token)
      case Action6 => isArray = true
      case Action7 => semanticStack.push(token)
      case Action9 => action9(token)
      case Action13 => semanticStack.push(token)
      case Action30 => action30(token)
      case Action31 => action31(token)
      case Action34 => action34()
      case Action40 => semanticStack.push(token)
      case Action42 => action42(token)
      //case Action43 =>
      case Action44 => action44(token)
     // case Action45 =>
      case Action46 => action46(token)
      case Action48 => action48()
      //case Action53 => action53(token)
      case Action55 => action55()
      case Action56 => action56()
      case _ => println("action not yet implemented")
    }
  }


  def gen(op: String, args: Any*): Quadruple = {
    val arguments = args.map(processArgument(_))
    val quad = arguments.length match {
      case 0 => Quadruple(op, "", "", "")
      case 1 => Quadruple(op, arguments(0), "", "")
      case 2 => Quadruple(op, arguments(0), arguments(1), "")
      case 3 => Quadruple(op, arguments(0), arguments(1), arguments(2))
      case _ => println("Too many arguments passed to gen"); Quadruple("","","","")
    }
    nextQuad = nextQuad + 1
    quadruples += quad
    quad
  }

  def processArgument(a: Any) = {
    a match {
      case _ => a.toString
    }
  }

  def backPatch(previous: Int, target: Int) = {
    val amended = quadruples(previous).copy(arg1 = target.toString)
    quadruples.update(previous, amended)
  }

  def typeCheck(id1: VariableEntry, id2: VariableEntry): Int = {
    (id1.dataType, id2.dataType) match {
      case ("integer", "integer") => 0
      case ("real", "real") => 1
      case ("real", "integer") => 2
      case ("integer", "real") => 3
    }
  }

  def create(name: String, dType: String): SymbolTableEntry = {
    if (global) {
      globalTable.insert(VariableEntry(name, globalMemory, dType))
      globalTable(name)
    }
    else {
      localTable.insert(VariableEntry(name, localMemory, dType))
      localTable(name)
    }
  }

  def symbolTable(): SymbolTable = {
    if (global) {
      globalTable
    }
    else {
      localTable
    }
  }

}


