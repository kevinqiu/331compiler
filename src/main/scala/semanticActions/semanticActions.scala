package semanticActions

import compiler._
import symbol._

import util.{Try, Success, Failure}
import collection.mutable.Stack

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

class SemanticActions {
  var semanticStack: SemanticStack[GrammarSymbol] = new SemanticStack
  var globalTable = new SymbolTable
  var constantTable = new SymbolTable
  var localTable = new SymbolTable
  var insert = false
  var isArray = false
  var global = false
  var globalMemory = 0
  var localMemory = 0

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
          globalTable.insert(VariableEntry(id, dataType))
          globalMemory = globalMemory + 1
        } else {
          localTable.insert(VariableEntry(id, dataType))
          localMemory = localMemory + 1
        }
      }

    }

    isArray = false
  }

  def execute(action: SemanticAction, token: Token) = {
    action match {
      case Action1 => insert = true
      case Action2 => insert = false
      case Action3 => action3(token)
      case Action4 => semanticStack.push(token)
      case Action5 =>
      case Action6 => isArray = true
      case Action7 => semanticStack.push(token)
      case Action13 => semanticStack.push(token)
      case _ => println("action not yet implemented")
    }
  }

  abstract class OpCode
  abstract class OpWord(value: String)
  case object CODE extends OpWord("CODE")

  def gen(tvi: OpCode) = {
  }
}
