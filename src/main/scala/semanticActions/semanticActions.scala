package semanticActions

import compiler._
import symbol._

import util.{Try, Success, Failure}
import collection.mutable.{Stack, ListBuffer}

class SemanticStack[A] extends Stack[A] {
  def popString(): String = {
    val element = this.pop()
    element match {
      case t: Token => t.value.toLowerCase
      case s: String => s
      case _ => ""
    }
  }
  def popDe(): DataEntry = {
    val element = this.pop()
    element match {
      case i: DataEntry => i
    }
  }
}

abstract class ETYPE
case class ARITHMETIC extends ETYPE
case class RELATIONAL extends ETYPE

case class Quadruple(opCode: String, arg1: String = "", arg2: String = "", result: String = "") {
  override def toString() = {
    (opCode + " " + arg1 + " " + arg2 + " " + result).trim
  }
}

class SemanticActions {
  var semanticStack: SemanticStack[Any] = new SemanticStack
  var globalTable = new SymbolTable
  var constantTable = new SymbolTable
  var localTable = new SymbolTable
  var quadruples = new ListBuffer[Quadruple]()
  var errors = new ListBuffer[CompilerError]
  var parmCount = new Stack[Int]()
  var nextParm = new Stack[SymbolTableEntry]()
  var eTrue = List[Int]()
  var eFalse = List[Int]()
  var skipElse = List[Int]()
  var insert = true
  var isArray = false
  var global = true
  var globalMemory = 0
  var localMemory = 0
  var globalStore = 0
  var localStore = 0
  var nextQuad = 1
  var currentFunction = FunctionEntry("", 0, 0, "")
  var tmp = 0

  quadruples += Quadruple("CODE")

  object DTYPE {
    val integer: String = INTCONSTANT().getType
    val real: String = REALCONSTANT().getType
  }

  def nextTmp() = {
    tmp = tmp + 1
    tmp.toString
  }

  def operatorOpCode(operator: Any) = {
    operator match {
      case addOps.+ => "add"
      case addOps.- => "sub"
      case mulOps.* => "mul"
      case mulOps.div => "div"
      case _ => ""
    }
  }

  def action3(token: Token) = {
    val dataType = semanticStack.popString()

    if (isArray) {

      val upperBound = semanticStack.popString().toInt
      val lowerBound = semanticStack.popString().toInt
      val mSize = upperBound - lowerBound + 1

      while (semanticStack.nonEmpty &&
        semanticStack.head.isInstanceOf[IDENTIFIER]) {
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

      while (semanticStack.nonEmpty && semanticStack.head.isInstanceOf[IDENTIFIER]) {
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
    while (semanticStack.nonEmpty && semanticStack.head.isInstanceOf[IDENTIFIER]) {
      val id = semanticStack.popString()
      //picked abritary 0 for address entry, not sure if correct, revist later
      globalTable.insert(VariableEntry(id, 0, "restricted"))
    }
    insert = false
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
    val eType = semanticStack.pop()
    val id1 = semanticStack.popDe()
    val offset = semanticStack.pop()
    val eType2 = semanticStack.pop()
    val id2 = semanticStack.popDe()
    if (eType != ARITHMETIC) {
      errors += GenericSemanticError("Error at "+ token)
    }
    val check = typeCheck(id1, id2)
    if (check == 3) {
      errors += GenericSemanticError("Error at "+ token)
    }
    if (check == 2) {
      val tmp = create(DTYPE.real)
      gen("ltof", tmp, id2)
      if (offset == null) {
        gen("move", id1, tmp)
      } else {
        gen("stor", id1, offset, tmp)
      }
    } else if (offset == null) {
      gen("move", id1, id2)
    } else {
      gen("stor", id2, offset, id1)
    }
  }
  //not complete
  /*
  def action33(token: Token) = {
    val eType = semanticStack.pop()
    if (eType != ARITHMETIC) {
      errors += GenericSemanticError("Error at "+ token)
    }
    val id = semanticStack.head
    if (id.isInstanceOf[] && id.dataType != DTYPE.integer) {
      errors += GenericSemanticError("Error at "+ token)
    }
    val tmp = create(DTYPE.integer)
    gen("sub", tmp1, 
  }*/

  //not complete
  def action34() = {
    if (semanticStack.nonEmpty &&
      semanticStack.head.isInstanceOf[FunctionEntry]) {
      //action52()
    } else {
      semanticStack.push(null)
    }
  }

  //not complete
  def action35(token: Token) = {
    parmCount.push(0)
    //symbolTable().lookup(id)
  }

  def action42(token: Token) = {
    val eType = semanticStack.pop()
    if (token == addOps.or) {
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

  //incomplete relational action
  def action43(token: Token) = {
    val eType = semanticStack.pop()
    val id1 = semanticStack.popDe()
    val operator = semanticStack.pop()
    val id2 = semanticStack.popDe()
    val tCheck = typeCheck(id1, id2)
    if (eType == RELATIONAL) {
    } else {
      if (eType != ARITHMETIC) {
      }
      tCheck match {
        case 0 => {
          val tmp = create(DTYPE.integer)
          gen(operatorOpCode(operator), id1, id2, tmp)
          semanticStack.push(tmp)
        }
        case 1 => {
          val tmp = create(DTYPE.real)
          gen("f" + operatorOpCode(operator), id1, id2, tmp)
          semanticStack.push(tmp)
        }
        case 2 => {
          val tmp1 = create(DTYPE.real)
          gen("ltof", id2, tmp1)
          val tmp2 = create(DTYPE.real)
          gen("f" + operatorOpCode(operator), id1, tmp1, tmp2)
          semanticStack.push(tmp2)
        }
        case 3 => {
          val tmp1 = create(DTYPE.real)
          gen("ltof", id1, tmp1)
          val tmp2 = create(DTYPE.real)
          gen("f" + operatorOpCode(operator), tmp1, id2, tmp2)
          semanticStack.push(tmp2)
        }
      }
    }
    semanticStack.push(ARITHMETIC)
  }

  //incomplete relational action
  def action44(token: Token) = {
    val eType = semanticStack.pop()
    semanticStack.push(token)
  }

  //incomplete relational action
  def action45(token: Token) = {
    val eType = semanticStack.pop()
    val id1 = semanticStack.popDe()
    val operator = semanticStack.pop()
    val id2 = semanticStack.popDe()
    val tCheck = typeCheck(id1, id2)
    //incomplete AND
    if (operator == mulOps.and) {
      if (eType != RELATIONAL) {
        errors += GenericSemanticError("Error at "+ token)
      }
    } else {
      if (eType != ARITHMETIC) {
        errors += GenericSemanticError("Error at "+ token)
      }
      if ((typeCheck(id1, id2) != 0) && (operator == mulOps.mod)) {
        errors += GenericSemanticError("Error at "+ token)
      }
      tCheck match {
        case 0 => {
          if (operator == mulOps.mod) {
            val tmp1 = create(DTYPE.integer)
            gen("move", id1, tmp1)
            val tmp2 = create(DTYPE.integer)
            gen("move", tmp1, tmp2)
            gen("sub", tmp2, id2, tmp1)
            gen("bge", tmp1, id2, nextQuad - 2)
            semanticStack.push(tmp1)
          } else if (operator == mulOps./) {
            val tmp1 = create(DTYPE.real)
            gen("ltof", id1, tmp1)
            val tmp2 = create(DTYPE.real)
            gen("ltof", id2, tmp2)
            val tmp3 = create(DTYPE.real)
            gen("fdiv", tmp2, tmp1, tmp3)
            semanticStack.push(tmp3)
          } else {
            val tmp = create(DTYPE.integer)
            gen(operatorOpCode(operator),id1, id2, tmp)
            semanticStack.push(tmp)
          }
        }
        case 1 => {
          if (operator == mulOps.div) {
            val tmp1 = create(DTYPE.integer)
            gen("ftol", id1, tmp1)
            val tmp2 = create(DTYPE.integer)
            gen("ftol", id2, tmp2)
            val tmp3 = create(DTYPE.integer)
            gen("div", tmp1, tmp2, tmp3)
            semanticStack.push(tmp3)
          } else {
            val tmp = create(DTYPE.real)
            gen("f"+operatorOpCode(operator), id1, id2, tmp)
            semanticStack.push(tmp)
          }
        }
        case 2 => {
          if (operator == mulOps.div) {
            val tmp1 = create(DTYPE.integer)
            gen("ftol", id1, tmp1)
            val tmp2 = create(DTYPE.integer)
            gen("div", tmp1, id2, tmp2)
            semanticStack.push(tmp2)
          } else {
            val tmp1 = create(DTYPE.real)
            gen("ftol", id1, tmp1)
            val tmp2 = create(DTYPE.real)
            gen("f"+operatorOpCode(operator), id1, tmp1, tmp2)
            semanticStack.push(tmp2)
          }
        }
        case 3 => {
          if (operator == mulOps.div) {
            val tmp1 = create(DTYPE.integer)
            gen("ftol", id2, tmp1)
            val tmp2 = create(DTYPE.integer)
            gen("div", id1, tmp1, tmp2)
            semanticStack.push(tmp2)
          } else {
            val tmp1 = create(DTYPE.real)
            gen("ltof", id1, tmp1)
            val tmp2 = create(DTYPE.real)
            gen("f"++operatorOpCode(operator), tmp1, id2, tmp2)
            semanticStack.push(tmp2)
          }

        }
        case _ =>
      }
    }
    semanticStack.push(ARITHMETIC)
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
            case None => {
              val const = ConstantEntry(id, t.getType)
              constantTable.insert(const)
              semanticStack.push(const)
            }
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
              val tmpEntry = create(entry.dataType)
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
    gen("free", globalMemory)
    backPatch(globalStore, globalMemory)
    gen("PROCEND")
  }

  def action56() = {
    gen("PROCBEGIN", "main")
    globalStore = nextQuad
    gen("alloc", "")
  }

  def execute(action: SemanticAction, token: Token) = {
//    println(action)
//    println(semanticStack)
//    println(token)
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
//      case Action31 => action31(token)
//      case Action34 => action34()
      case Action40 => semanticStack.push(token)
      case Action42 => action42(token)
      case Action43 => action43(token)
      case Action44 => action44(token)
      case Action45 => action45(token)
      case Action46 => action46(token)
      case Action48 => action48()
      //case Action53 => action53(token)
      case Action55 => action55()
      case Action56 => action56()
      case _ => //println("action not yet implemented")
    }
  }


  def addQuadruple(quad: Quadruple) = {
    nextQuad = nextQuad + 1
    quadruples += quad
    quad
  }

  def gen(op: String): Quadruple = {
    addQuadruple(Quadruple(op))
  }

  def gen(op: String, arg1: Any): Quadruple = {
    addQuadruple(Quadruple(op, processArgument(arg1)))
  }

  def gen(op: String, arg1: Any, arg2: Any): Quadruple = {
    addQuadruple(Quadruple(op, processArgument(arg1), processArgument(arg2)))
  }

  def gen(op: String, arg1: Any, arg2: Any, arg3: Any): Quadruple = {
    addQuadruple(Quadruple(op, processArgument(arg1), processArgument(arg2),
      processArgument(arg3)))
  }

  def genOffset(address: Int): String = {
    if (global) "_" + address else "%" + address
  }

  def processArgument(a: Any) = {
    a match {
      case v: VariableEntry => genOffset(v.address)
      case c: ConstantEntry => c.name
      case _ => a.toString
    }
  }

  def backPatch(previous: Int, target: Int) = {
    val amended = quadruples(previous).copy(arg1 = target.toString)
    quadruples.update(previous, amended)
  }

  def typeCheck(id1: DataEntry, id2: DataEntry): Int = {
    (id1.dataType, id2.dataType) match {
      case (DTYPE.integer, DTYPE.integer) => 0
      case (DTYPE.real, DTYPE.real) => 1
      case (DTYPE.real, DTYPE.integer) => 2
      case (DTYPE.integer, DTYPE.real) => 3
    }
  }

  def create(dType: String): SymbolTableEntry = {
    val name = "$$" + "TEMP" + nextTmp()
    if (global) {
      globalTable.insert(VariableEntry(name, -globalMemory, dType))
      globalMemory = globalMemory + 1
      globalTable(name)
    }
    else {
      localTable.insert(VariableEntry(name, -localMemory, dType))
      localMemory = localMemory + 1
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

  def semanticStackDump() = {
    println("Contents of semantic stack")
    semanticStack.foreach(println(_))
  }

}


