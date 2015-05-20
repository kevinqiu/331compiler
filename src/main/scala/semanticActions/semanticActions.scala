package semanticActions

import compiler._
import symbol._

import util.{Try, Success, Failure}
import collection.mutable.{Stack, ListBuffer}
import scala.reflect.ClassTag

class SemanticStack[A] extends Stack[A] {

  //TODO: return option instead of generating match error
  def popT[T: ClassTag]() = {
    val element = this.pop()
    element match {
      case e: T => e
    }
  }

  def headT[T: ClassTag]() = {
    val element = this.head
    element match {
      case e: T => e
    }
  }

  def lastT[T: ClassTag]() = {
    val element = this.last
    element match {
      case e: T => e
    }
  }
}

abstract class ETYPE
case object ARITHMETIC extends ETYPE
case object RELATIONAL extends ETYPE

case class Quadruple(opCode: String, arg1: String = "", arg2: String = "", arg3: String = "") {
  override def toString() = {
    def printArg(arg: String) = {
      if (arg.nonEmpty) {
        arg + ","
      } else {
        ""
      }
    }
    val raw = opCode match {
      case _ => (opCode + " " + printArg(arg1) + " " + printArg(arg2) + " " + arg3).trim
    }
    if (raw.last == ',') raw.init else raw
  }
}

class SemanticActions {
  var semanticStack: SemanticStack[Any] = new SemanticStack
  var globalTable = new SymbolTable
  var constantTable = new SymbolTable
  var localTable = new SymbolTable
  var quadruples = new ListBuffer[Quadruple]()
  var parmCount = new Stack[Int]()
  var nextParm = new Stack[List[SymbolTableEntry with DataEntry]]()
  var skipElse = List[Int]()
  var insert = true
  var isArray = false
  var global = true
  var globalMemory = 0
  var localMemory = 0
  var globalStore = 0
  var localStore = 0
  var nextQuad = 1
  var currentFunction = FunctionEntry("", 0, List(), "", "")
  var tmp = 0

  globalTable.insert(ProcedureEntry("write", 0, List[SymbolTableEntry with DataEntry]()))
  globalTable.insert(ProcedureEntry("read", 0, List[SymbolTableEntry with DataEntry]()))

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
      case mulOps./ => "div"
    }
  }

  def relOpCode(relOp: RELOP) = {
    relOp match {
      case relOps.equals => "beq"
      case relOps.<> => "bne"
      case relOps.<= => "ble"
      case relOps.< => "blt"
      case relOps.>= => "bge"
      case relOps.> => "bgt"
    }
  }

  def action3(token: Token) = {
    val dataType = semanticStack.popT[Token]().toString

    if (isArray) {

      val upperBound = semanticStack.popT[Token with CONSTANT]().value.toInt
      val lowerBound = semanticStack.popT[Token with CONSTANT]().value.toInt
      val mSize = upperBound - lowerBound + 1

      while (semanticStack.nonEmpty &&
        semanticStack.head.isInstanceOf[IDENTIFIER]) {
        val id = semanticStack.popT[IDENTIFIER]().value
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
        val id = semanticStack.popT[IDENTIFIER]().value
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
    Success("SA complete")
  }

  def action5(token: Token) = {
    insert = false
    val entry = semanticStack.pop()
    val id = entry match {
      case i: IDENTIFIER => i.value
      case s: SymbolTableEntry => s.name
    }
    gen("PROCBEGIN", id)
    localStore = nextQuad
    gen("alloc", "_")
    Success("SA complete")
  }

  def action9(token: Token) = {
    while (semanticStack.nonEmpty && semanticStack.head.isInstanceOf[IDENTIFIER]) {
      val id = semanticStack.popT[IDENTIFIER]().value
      //picked abritary 0 for address entry, not sure if correct, revist later
      globalTable.insert(VariableEntry(id, 0, "restricted"))
    }
    insert = false
    gen("call", "main", 0)
    gen("exit")
    Success("SA complete")
  }

  def action11(token: Token) = {
    global = true
    currentFunction = FunctionEntry("", 0, List(), "", "")
    gen("free", localMemory)
    gen("PROCEND")
    backPatch(List(localStore), localMemory)
    Success("SA complete")
  }

  //not complete, go over
  def action15(token: Token) = {
    val name = token.value
    val result = create(DTYPE.integer)
    val fEntry = FunctionEntry(name, 0, List(), result.name, "")
    globalTable.insert(fEntry)
    localMemory = 0
    global = false
    semanticStack.push(fEntry)
    Success("SA complete")
  }

  def action16(token: Token) = {
    val eType = semanticStack.popT[DataType]().toString
    val entry = semanticStack.popT[SymbolTableEntry]()
    for {
      newEntry <- globalTable.lookup(entry.name) match {
        case Some(f: FunctionEntry) => Success(f.copy(dataType = eType))
        case _ => Failure(GenericSemanticError("Error at "+ token))
      }
      newResult <- globalTable.lookup(newEntry.result) match {
        case Some(v: VariableEntry) => Success(v.copy(dataType = eType))
        case _ => Failure(GenericSemanticError("Error at "+ token))
      }
    } yield {
      currentFunction = newEntry
      globalTable.insert(newEntry)
      globalTable.insert(newResult)
      semanticStack.push(newEntry)
      "SA complete"
    }
  }

  def action17(token: Token) = {
    val name = token.value
    val pEntry = ProcedureEntry(name, 0, List())
    globalTable.insert(pEntry)
    global = false
    localMemory = 0
    semanticStack.push(pEntry)
    Success("SA complete")
  }

  def action19(token: Token) = {
    parmCount.push(0)
    Success("SA complete")
  }

  def action20(token: Token) = {
    val parms = parmCount.pop()
    val entry = semanticStack.popT[SymbolTableEntry]()
    val newEntry = globalTable.lookup(entry.name) match {
      case Some(p: ProcedureEntry) => Success(p.copy(numberOfParameters = parms))
      case Some(f: FunctionEntry) => Success(f.copy(numberOfParameters = parms))
      case _ => Failure(GenericSemanticError("Error at "+ token))
    }
    newEntry.map(ne => {
      globalTable.insert(ne)
      semanticStack.push(ne)
      "SA complete"
    })
  }

  def action21(token: Token) = {
    val iType = semanticStack.popT[Token]()
    var params = ListBuffer[SymbolTableEntry with DataEntry]()
    while (semanticStack.nonEmpty &&
      !semanticStack.head.isInstanceOf[SymbolTableEntry with Params]) {
      val entry = if (isArray) {
        val ub = semanticStack.popT[Token with CONSTANT]().value.toInt
        val lb = semanticStack.popT[Token with CONSTANT]().value.toInt
        val id = semanticStack.popT[IDENTIFIER]()
        ArrayEntry(id.value, localMemory, iType.toString, ub, lb, true)
      } else {
        val id = semanticStack.popT[IDENTIFIER]()
        VariableEntry(id.value, localMemory, iType.toString, true)
      }
      entry +=: params
    }
    //params were popped in reverse order so we have to wait until all are
    //popped before we can assign them the correct memory address
    params.map(p => {
      val entry = p match {
        case a: ArrayEntry => a.copy(address = localMemory)
        case v: VariableEntry => v.copy(address = localMemory)
      }
      symbolTable().insert(entry)
      localMemory = localMemory + 1
      parmCount.push(parmCount.pop() + 1)
      entry
    })
    val procedure = semanticStack.popT[SymbolTableEntry]()
    val newEntry = globalTable.lookup(procedure.name) match {
      case Some(p: ProcedureEntry) => Success(p.copy(parameterInfo = (p.parameterInfo ++ params).toList))
      case Some(f: FunctionEntry) => Success(f.copy(parameterInfo = (f.parameterInfo ++ params).toList))
      case _ => Failure(GenericSemanticError("Error at "+ token))
    }
    newEntry.map(ne => {
      globalTable.insert(ne)
      semanticStack.push(ne)
      isArray = false
      "SA complete"
    })
  }

  def action22(token: Token) = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType != RELATIONAL) {
      Failure(GenericSemanticError("Error at "+ token))
    } else {
      //pop false to get to true
      val eFalse = semanticStack.popT[List[Int]]()
      val eTrue = semanticStack.headT[List[Int]]()
      backPatch(eTrue, nextQuad)
      //put false back
      semanticStack.push(eFalse)
      Success("SA complete")
    }
  }

  def action24(token: Token) = {
    val beginLoop = nextQuad
    semanticStack.push(beginLoop)
    Success("SA complete")
  }

  def action25(token: Token) = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType != RELATIONAL) {
      Failure(GenericSemanticError("Error at "+ token))
    } else {
      //pop false to get to true
      val eFalse = semanticStack.popT[List[Int]]()
      val eTrue = semanticStack.headT[List[Int]]()
      backPatch(eTrue, nextQuad)
      //put false back
      semanticStack.push(eFalse)
      Success("SA complete")
    }
  }

  def action26(token: Token) = {
    val eFalse = semanticStack.popT[List[Int]]()
    val eTrue = semanticStack.popT[List[Int]]()
    val beginLoop = semanticStack.popT[Integer]()

    gen("goto", beginLoop)
    backPatch(eFalse, nextQuad)
    Success("SA complete")
  }

  def action27(token: Token) = {
    val skipElse = makeList(nextQuad)
    gen("goto", "_")
    backPatch(semanticStack.headT[List[Int]](), nextQuad)
    semanticStack.push(skipElse)
    Success("SA complete")
  }

  def action28(token: Token) = {
    val skipElse = semanticStack.popT[List[Int]]()
    val eFalse = semanticStack.popT[List[Int]]()
    val eTrue = semanticStack.popT[List[Int]]()
    backPatch(skipElse, nextQuad)
    Success("SA complete")
  }

  def action29(token: Token) = {
    val eFalse = semanticStack.popT[List[Int]]()
    val eTrue = semanticStack.popT[List[Int]]()

    backPatch(eFalse, nextQuad)
    Success("SA complete")
  }

  def action30(token: Token) = {
    val id = token.value
    val result = lookup(id)
    result match {
      case Some(res) => {
        semanticStack.push(res)
        semanticStack.push(ARITHMETIC)
        Success("SA complete")
      }
      case _ => Failure(UndeclaredVariable("Identifier " + id +" used before declaration"))
    }
  }

  def action31(token: Token) = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType != ARITHMETIC) {
      Failure(GenericSemanticError("Error at "+ token))
    } else {
      val id2 = semanticStack.popT[DataEntry]()
      val offset = semanticStack.pop()
      val eType2 = semanticStack.popT[ETYPE]()
      val id1 = semanticStack.popT[DataEntry]()
      val check = typeCheck(id1, id2)
      if (check == 3) {
        Failure(GenericSemanticError("Error at "+ token))
      } else {
        if (check == 2) {
          val tmp = create(DTYPE.real)
          gen("ltof", id2, tmp)
          if (offset == null) {
            gen("move", tmp, id1)
          } else {
            gen("stor", tmp, offset, id1)
          }
        } else if (offset == null) {
          gen("move", id2, id1)
        } else {
          gen("stor", id2, offset, id1)
        }
        Success("SA complete")
      }
    }
  }

  def action32(token: Token) = {
    val entry = lookup(token.value)
    entry match {
      case Some(a: ArrayEntry) => Success("SA complete")
        //expected array ID
      case _ => Failure(GenericSemanticError("Error at "+ token))
    }
  }


  def action33(token: Token): Try[String] = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType != ARITHMETIC) {
      Failure(GenericSemanticError("Error at "+ token))
    } else {
      val id = semanticStack.headT[DataEntry]
      if (id.dataType != DTYPE.integer) {
        Failure(GenericSemanticError("Error at "+ token))
      } else {
        val tmp = create(DTYPE.integer)
        val tmp1 = semanticStack.pop()
        val arrayEntry = semanticStack.find(_.isInstanceOf[ArrayEntry]) match {
          case Some(a: ArrayEntry) => a
          case _ => return Failure(GenericSemanticError("Error at "+ token))
        }
        gen("sub", tmp1, arrayEntry.lowerBound, tmp)
        semanticStack.push(tmp)
        Success("SA complete")
      }
    }
  }

  //different from specifications? change on 17/5/15
  def action34(token: Token) = {
    if (semanticStack.nonEmpty &&
      semanticStack.head.isInstanceOf[FunctionEntry]) {
      action52(token)
      Success("SA complete")
    } else {
      semanticStack.push(null)
      Success("SA complete")
    }
  }

  def action35(token: Token) = {
    parmCount.push(0)
    val parmInfo = globalTable.lookup(token.value) match {
      case Some(p: ProcedureEntry) => Success(p.parameterInfo)
      case _ => Failure(GenericSemanticError("Error at "+ token))
    }
    parmInfo.map(pi => {
      nextParm.push(pi)
      "SA complete"
    })
  }

  def action36(token: Token) = {
    val eType = semanticStack.pop()
    val id = semanticStack.popT[SymbolTableEntry with Params]()
    if (id.numberOfParameters != 0) {
      Failure(GenericSemanticError("Error at "+ token))
    } else {
      gen("call", id.name, 0)
      Success("SA complete")
    }
  }

  //not finished, check for variable, constant, ect. not implemented
  //array param not implemented
  def action37(token: Token): Try[String] = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType != ARITHMETIC) {
      Failure(GenericSemanticError("Error at "+ token))
    } else if (false) {
      Failure(GenericSemanticError("Error at "+ token))
    } else {
      parmCount.push(parmCount.pop() + 1)
      val pc = parmCount.head
      val procOrFun = semanticStack.find(_.isInstanceOf[SymbolTableEntry with Params]) match {
        case Some(pf: SymbolTableEntry with Params) => pf
        case _ => return Failure(GenericSemanticError("Error at "+ token))
      }
      if (procOrFun.name != "read" && procOrFun.name != "write") {
        val id = semanticStack.headT[SymbolTableEntry with DataEntry]()
        val next = nextParm.pop()
        if (pc > procOrFun.numberOfParameters) {
          Failure(GenericSemanticError("Error at "+ token))
        } else if (id.dataType != next.head.dataType) {
          Failure(GenericSemanticError("Error at "+ token))
        } else {
          nextParm.push(next.tail)
          Success("SA complete")
        }
      } else {
        Success("SA complete")
      }
    }
  }

  def action38(token: Token) = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType != ARITHMETIC) {
      Failure(GenericSemanticError("Error at "+ token))
    }
    semanticStack.push(token)
    Success("SA complete")
  }

  def action39(token: Token) = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType != ARITHMETIC) {
      Failure(GenericSemanticError("Error at "+ token))
    } else {
      val id2 = semanticStack.popT[DataEntry]()
      val relOp = semanticStack.popT[RELOP]()
      val id1 = semanticStack.popT[DataEntry]()
      val check = typeCheck(id1, id2)
      if (check == 2) {
        val tmp = create(DTYPE.real)
        gen("ltof", id2, tmp)
        gen(relOpCode(relOp), id1, tmp, "_")
      } else if (check == 3) {
        val tmp = create(DTYPE.real)
        gen("ltof", id1, tmp)
        gen(relOpCode(relOp), tmp, id2, "_")
      } else {
        gen(relOpCode(relOp), id1, id2, "_")
      }
      gen("goto", "_")
      val eTrue = makeList(nextQuad - 2)
      val eFalse = makeList(nextQuad - 1)
      semanticStack.push(eTrue)
      semanticStack.push(eFalse)
      semanticStack.push(RELATIONAL)
      Success("SA complete")
    }
  }

  def action41(token: Token) = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType != ARITHMETIC) {
      Failure(GenericSemanticError("Error at "+ token))
    } else {
      val id = semanticStack.popT[SymbolTableEntry with DataEntry]()
      val sign = semanticStack.popT[Token]()
      if (sign == UNARYMINUS) {
        val tmp = create(id.dataType)
        gen("uminus", id, tmp)
        semanticStack.push(tmp)
      } else {
        semanticStack.push(id)
      }
      semanticStack.push(ARITHMETIC)
      Success("SA complete")
    }
  }

  //not finished
  def action42(token: Token): Try[String] = {
    val eType = semanticStack.popT[ETYPE]()
    if (token == addOps.or) {
      if (eType != RELATIONAL) {
        return Failure(GenericSemanticError("Error at "+ token))
      }
      val eFalse = semanticStack.headT[List[Int]]()
      backPatch(eFalse, nextQuad)
    } else if (eType == ARITHMETIC) {

    }
    semanticStack.push(token)
    Success("SA complete")
  }

  def action43(token: Token): Try[String] = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType == RELATIONAL) {
      //not sure of order of popping, check when there's an example
      val e2False = semanticStack.popT[List[Int]]()
      val e2True = semanticStack.popT[List[Int]]()
      val operator = semanticStack.popT[Token]()
      val e1False = semanticStack.popT[List[Int]]()
      val e1True = semanticStack.popT[List[Int]]()
      if (operator == addOps.or) {
        val eTrue = merge(e1True, e2True)
        val eFalse = merge(e2False, e2False)
        semanticStack.push(eTrue)
        semanticStack.push(eFalse)
        semanticStack.push(RELATIONAL)
      }
    } else {
      val id2 = semanticStack.popT[DataEntry]()
      val operator = semanticStack.popT[Token]()
      val id1 = semanticStack.popT[DataEntry]()
      val tCheck = typeCheck(id1, id2)
      if (eType != ARITHMETIC) {
        return Failure(GenericSemanticError("Error at "+ token))
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
      semanticStack.push(ARITHMETIC)
    }
    Success("SA complete")
  }

  def action44(token: Token) = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType == RELATIONAL && token == mulOps.and) {
      //pop false to get to true
      val eFalse = semanticStack.popT[List[Int]]()
      val eTrue = semanticStack.headT[List[Int]]()
      backPatch(eTrue, nextQuad)
      //put false back
      semanticStack.push(eFalse)
    }
    semanticStack.push(token)
    Success("SA complete")
  }

  //check order of popping
  def action45(token: Token): Try[String] = {
    val eType = semanticStack.popT[ETYPE]()
    //check false/true order
    if (eType == RELATIONAL) {
      val e2False = semanticStack.popT[List[Int]]()
      val e2True = semanticStack.popT[List[Int]]()
      val operator = semanticStack.popT[Token]()
      if (operator != mulOps.and) {
        return Failure(GenericSemanticError("Error at "+ token))
      }
      val e1False = semanticStack.popT[List[Int]]()
      val e1True = semanticStack.popT[List[Int]]()
      val eTrue = e2True
      val eFalse = merge(e1False, e2False)
      semanticStack.push(eTrue)
      semanticStack.push(eFalse)
      semanticStack.push(RELATIONAL)
    } else {
      val id2 = semanticStack.popT[DataEntry]()
      val operator = semanticStack.pop()
      val id1 = semanticStack.popT[DataEntry]()
      val tCheck = typeCheck(id1, id2)
      if (eType != ARITHMETIC) {
        return Failure(GenericSemanticError("Error at "+ token))
      }
      if ((typeCheck(id1, id2) != 0) && (operator == mulOps.mod)) {
        return Failure(GenericSemanticError("Error at "+ token))
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
            gen("fdiv", tmp1, tmp2, tmp3)
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
      semanticStack.push(ARITHMETIC)
    }
    Success("SA complete")
  }

  def action46(token: Token): Try[String] = {
    def parseNum(num: String) = {
      if (num.contains("e")) num.toDouble.toLong.toString else num
    }
    val id = token.value
    if (token.isInstanceOf[IDENTIFIER]) {
      val result = lookup(id)
      result.map((res) => {
        semanticStack.push(res)
      })
      if (result.isEmpty) return Failure(UndeclaredVariable("Identifier " + id +" used before declaration"))
    } else {
      token match {
        case t: CONSTANT => {
          val num =  parseNum(id)
          val result = constantTable.lookup(num)
          result match {
            case Some(res) => semanticStack.push(res)
            case None => {
              val const = ConstantEntry(num, t.getType)
              constantTable.insert(const)
              semanticStack.push(const)
            }
          }
        }
        case _ =>
      }
    }
    semanticStack.push(ARITHMETIC)
    Success("SA complete")
  }

  def action47(token: Token): Try[String] = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType != RELATIONAL) {
      return Failure(GenericSemanticError("Error at "+ token))
    }
    val eFalse = semanticStack.popT[List[Int]]()
    val eTrue = semanticStack.popT[List[Int]]()
    //swap True and False
    semanticStack.push(eFalse)
    semanticStack.push(eTrue)
    semanticStack.push(RELATIONAL)
    Success("SA complete")
  }

  def action48(token: Token): Try[String] = {
    if (semanticStack.head != null) {
      val offset = semanticStack.popT[SymbolTableEntry with DataEntry]()
      if (offset.dataType != DTYPE.integer) return Failure(GenericSemanticError("Error at "+ token))
      val eType = semanticStack.popT[ETYPE]()
      val id = semanticStack.popT[SymbolTableEntry with DataEntry]()
      val tmpEntry = create(id.dataType)
      gen("load", id, offset, tmpEntry)
      semanticStack.push(tmpEntry)
      semanticStack.push(ARITHMETIC)
      Success("SA complete")
    } else {
      semanticStack.pop()
      Success("SA complete")
    }
  }

  def action49(token: Token): Try[String] = {
    val eType = semanticStack.head
    val id = globalTable.lookup(token.value).head
    if (!id.isInstanceOf[Params]) {
      Failure(GenericSemanticError("Error at "+ token))
    } else {
      parmCount.push(0)
      val np = id match {
        case func: Params => func.parameterInfo
      }
      nextParm.push(np)
      Success("SA complete")
    }
  }

  def action50(token: Token) = {
    var params = new ListBuffer[VariableEntry]()
    while (semanticStack.head.isInstanceOf[VariableEntry]) {
      val id = semanticStack.popT[VariableEntry]()
      localMemory = localMemory + 1
      params += id
    }
    params.reverse.foreach(id => gen("param", id))
    val pc = parmCount.pop()
    val np = nextParm.pop()
    val eType = semanticStack.pop()
    val func = semanticStack.popT[FunctionEntry]()
    if (pc > func.numberOfParameters) {
      Failure(GenericSemanticError("Error at "+ token))
    } else {
      gen("call", func.name, pc)
      val tmp = create(func.dataType)
      gen("move", func, tmp)
      semanticStack.push(tmp)
      semanticStack.push(ARITHMETIC)
      Success("SA complete")
    }
  }

  def action51(token: Token): Try[String] = {
    val proc = semanticStack.find(_.isInstanceOf[ProcedureEntry]) match {
      case Some(p: ProcedureEntry) => p
      case _ => return Failure(GenericSemanticError("Error at "+ token))
    }
    if (proc.name == "read") {
      action51Read()
    } else if (proc.name == "write") {
      action51Write()
    } else {
      var params = new ListBuffer[SymbolTableEntry with Address]()
      while (semanticStack.head.isInstanceOf[SymbolTableEntry with Address]) {
        val id = semanticStack.popT[SymbolTableEntry with Address]()
        localMemory = localMemory + 1
        params += id
      }
      params.reverse.foreach(id => gen("param", id))
      val pc = parmCount.pop()
      val np = nextParm.pop()
      val eType = semanticStack.pop()
      //pop id, but we already have it above since we had to determine in advance
      //if it is read/wrtie
      semanticStack.pop()
      gen("call", proc.name, pc)
    }
    Success("SA complete")
  }

  def action51Write() = {
    var params = new ListBuffer[SymbolTableEntry with DataEntry]()
    while (semanticStack.head.isInstanceOf[SymbolTableEntry with DataEntry]) {
      val id = semanticStack.popT[SymbolTableEntry with DataEntry]()
      params += id
    }
    params.reverse.foreach(id => {
      gen("print", "\"<"+id.name+ "> = \"")
      if (id.dataType == DTYPE.real) {
        gen("foutp", id)
      } else {
        gen("outp", id)
      }
      gen("newl")
    })
    val eType = semanticStack.popT[ETYPE]()
    val id = semanticStack.pop()
    val pc = parmCount.pop()
    Success("SA complete")
  }

  def action51Read() = {
    var params = new ListBuffer[SymbolTableEntry with DataEntry]()
    while (semanticStack.head.isInstanceOf[SymbolTableEntry with DataEntry]) {
      val id = semanticStack.popT[SymbolTableEntry with DataEntry]()
      params += id
    }
    params.reverse.foreach(id => {
      if (id.dataType == DTYPE.real) {
        gen("finp", id)
      } else {
        gen("inp", id)
      }
    })
    val eType = semanticStack.popT[ETYPE]()
    val id = semanticStack.pop()
    val pc = parmCount.pop()
    Success("SA complete")
  }

  def action52(token: Token): Try[String] = {
    val eType = semanticStack.pop()
    val func = semanticStack.pop() match {
      case f: FunctionEntry => f
      case _ => return Failure(GenericSemanticError("Error at "+ token))
    }
    if (func.numberOfParameters > 0) {
      Failure(GenericSemanticError("Error at "+ token))
    } else {
      gen("call", func.name, 0)
      val tmp = create(func.dataType)
      gen("move", func, tmp)
      semanticStack.push(tmp)
      semanticStack.push(ARITHMETIC)
      Success("SA complete")
    }
  }

  def action53(token: Token): Try[String] = {
    val entry = lookup(token.value)
    entry match {
      case Some(e: FunctionEntry) => {
        val eType = semanticStack.popT[ETYPE]()
        val id = semanticStack.popT[SymbolTableEntry]
        if (id.name != currentFunction.name) {
          //non-matching functions
          Failure(GenericSemanticError("Error at "+ token))
        } else {
          semanticStack.push(globalTable.lookup(e.result).head)
          semanticStack.push(ARITHMETIC)
          Success("SA complete")
        }
      }
      case _ => Success("SA complete")
    }
  }

  def action54(token: Token) = {
    val entry = lookup(token.value)
    entry match {
      case Some(a: ProcedureEntry) => Success("SA complete")
      //expected array ID
      case _ => Failure(GenericSemanticError("Error at "+ token))
    }
  }

  def action55() = {
    gen("free", globalMemory)
    backPatch(List(globalStore), globalMemory)
    gen("PROCEND")
    Success("SA complete")
  }

  def action56() = {
    gen("PROCBEGIN", "main")
    globalStore = nextQuad
    gen("alloc", "_")
    Success("SA complete")
  }

  def execute(action: SemanticAction, token: Token): Try[String] = {
    println(action)
    println(semanticStack)
    println(token)
    val result = action match {
      case Action1 => insert = true; Success("SA complete")
      case Action2 => insert = false; Success("SA complete")
      case Action3 => action3(token)
      case Action4 => semanticStack.push(token); Success("SA complete")
      case Action5 => action5(token)
      case Action6 => isArray = true; Success("SA complete")
      case Action7 => semanticStack.push(token); Success("SA complete")
      case Action9 => action9(token)
      case Action11 => action11(token)
      case Action13 => semanticStack.push(token); Success("SA complete")
      case Action15 => action15(token)
      case Action16 => action16(token)
      case Action17 => action17(token)
      case Action19 => action19(token)
      case Action20 => action20(token)
      case Action21 => action21(token)
      case Action22 => action22(token)
      case Action24 => action24(token)
      case Action25 => action25(token)
      case Action26 => action26(token)
      case Action27 => action27(token)
      case Action28 => action28(token)
      case Action29 => action29(token)
      case Action30 => action30(token)
      case Action31 => action31(token)
      case Action32 => action32(token)
      case Action33 => action33(token)
      case Action34 => action34(token)
      case Action35 => action35(token)
      case Action36 => action36(token)
      case Action37 => action37(token)
      case Action38 => action38(token)
      case Action39 => action39(token)
      case Action40 => semanticStack.push(token); Success("SA complete")
      case Action41 => action41(token)
      case Action42 => action42(token)
      case Action43 => action43(token)
      case Action44 => action44(token)
      case Action45 => action45(token)
      case Action46 => action46(token)
      case Action47 => action47(token)
      case Action48 => action48(token)
      case Action49 => action49(token)
      case Action50 => action50(token)
      case Action51 => action51(token)
      case Action52 => action52(token)
      case Action53 => action53(token)
      case Action54 => action54(token)
      case Action55 => action55()
      case Action56 => action56()
      case _ => Failure(GenericSemanticError("SA doesn't exist"))
    }
    //temporary fix to handle match exceptions from popT & family
    //ideally no exceptions thrown when error is encountered
    Try(result).flatten
  }


  def addQuadruple(quad: Quadruple) = {
    nextQuad = nextQuad + 1
    quadruples += quad
    quad
  }

  def gen(op: String, args: Any*): Quadruple = {
    (op, args.headOption) match {
      //special case param
      case ("param", Some(s: SymbolTableEntry with Address with Param)) => {
        val reifiedAddresss = if (s.param) "%" + s.address.abs else "@" + getReifiedAddress(s)
        addQuadruple(Quadruple(op, reifiedAddresss))
      }
      case _ => {
        val arguments = args.map(processArgument(_))
        val quad = arguments.length match {
          case 0 => Some(Quadruple(op))
          case 1 => Some(Quadruple(op, arguments(0)))
          case 2 => Some(Quadruple(op, arguments(0), arguments(1)))
          case 3 => Some(Quadruple(op, arguments(0), arguments(1), arguments(2)))
          case _ => None
        }
        quad.map(addQuadruple).head
      }
    }
  }

  def getReifiedAddress(entry: SymbolTableEntry with Address): String = {
    val addressAbs = entry.address.abs
    if (localTable.lookup(entry.name).nonEmpty) {
      "%" + addressAbs
    } else {
      "_" + addressAbs
    }
  }

  def genOffset(entry: SymbolTableEntry with Address, param: Boolean): String = {
    val addressAbs = entry.address.abs
    //param special case, always local
    if (param) "^%" + addressAbs else getReifiedAddress(entry)
  }

  def addConstant(id: ConstantEntry) = {
    val tmp = create(id.dataType)
    gen("move", id.name, tmp)
    genOffset(tmp, false)
  }

  def processArgument(a: Any) = {
    //param false as convenience, eventually need to resolve param to corect address with @
    val param = false
    a match {
      case p: SymbolTableEntry with Param with Address => genOffset(p, p.param)
      case a: SymbolTableEntry with Address => genOffset(a, false)
      case c: ConstantEntry => addConstant(c)
      case f: FunctionEntry => globalTable.lookup(f.result).head match {
        case t: SymbolTableEntry with Address => genOffset(t, false) }
      case _ => a.toString
    }
  }

  def makeList(i: Int): List[Int] = {
    List(i)
  }

  def merge(p1: List[Int], p2: List[Int]): List[Int] = {
    p1 ::: p2
  }

  def backPatch(previousList: List[Int], target: Int) = {
    def replaceTarget(p: Quadruple) = {
      if (p.arg2.isEmpty) {
        p.copy(arg1 = target.toString)
      } else if (p.arg3.isEmpty) {
        p.copy(arg2 = target.toString)
      } else {
        p.copy(arg3 = target.toString)
      }
    }
    previousList.foreach( previous => {
      val amended = replaceTarget(quadruples(previous))
      quadruples.update(previous, amended)
    })
  }

  def typeCheck(id1: DataEntry, id2: DataEntry): Int = {
    (id1.dataType, id2.dataType) match {
      case (DTYPE.integer, DTYPE.integer) => 0
      case (DTYPE.real, DTYPE.real) => 1
      case (DTYPE.real, DTYPE.integer) => 2
      case (DTYPE.integer, DTYPE.real) => 3
    }
  }

  def create(dType: String): VariableEntry = {
    val name = "$$" + "TEMP" + nextTmp()
    if (global) {
      val entry = VariableEntry(name, -globalMemory, dType)
      globalTable.insert(entry)
      globalMemory = globalMemory + 1
      entry
    }
    else {
      val entry = VariableEntry(name, -localMemory, dType)
      localTable.insert(entry)
      localMemory = localMemory + 1
      entry
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

  def lookup(name: String): Option[Any] = {
    symbolTable.lookup(name) match {
      case None => globalTable.lookup(name)
      case e => e
    }
  }

  def getTvi(): String = {
    val body = quadruples.tail.foldLeft((1, "")){case ((index, output), quad) => {
      (index + 1, output + "\n" + index + ":   " + quad.toString)
    }}._2
    quadruples.head.toString + body
  }

  def semanticStackDump() = {
    println("Contents of semantic stack")
    semanticStack.foreach(println(_))
  }

}


