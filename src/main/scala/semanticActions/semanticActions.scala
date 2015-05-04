package semanticActions

import compiler._
import symbol._

import util.{Try, Success, Failure}
import collection.mutable.{Stack, ListBuffer}
import scala.reflect.ClassTag

class SemanticStack[A] extends Stack[A] {
  //holdover from before reflection, replace ASAP
  def popString(): String = {
    val element = this.pop()
    element match {
      case t: Token => t.value.toLowerCase
      case s: String => s
      case _ => ""
    }
  }

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
    (opCode + " " + arg1 + " " + arg2 + " " + arg3).trim
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
      val id = semanticStack.popString()
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
    localTable.insert(fEntry)
    localMemory = 0
    global = false
    semanticStack.push(fEntry)
    Success("SA complete")
  }

  def action16(token: Token) = {
    val eType = semanticStack.popT[DataType]().toString
    val entry = semanticStack.popT[SymbolTableEntry]()
    val newEntry = globalTable(entry.name) match {
      case f: FunctionEntry => f.copy(dataType = eType)
    }
    val newResult = globalTable(newEntry.result) match {
      case v: VariableEntry => v.copy(dataType = eType)
    }
    currentFunction = newEntry
    globalTable.insert(newEntry)
    globalTable.insert(newResult)
    semanticStack.push(newEntry)
    Success("SA complete")
  }

  def action17(token: Token) = {
    val name = token.value
    val pEntry = ProcedureEntry(name, 0, List())
    symbolTable().insert(pEntry)
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
    val newEntry = globalTable(entry.name) match {
      case p: ProcedureEntry => p.copy(numberOfParameters = parms)
      case f: FunctionEntry => f.copy(numberOfParameters = parms)
    }
    globalTable.insert(newEntry)
    semanticStack.push(newEntry)
    Success("SA complete")
  }

  //not complete
  def action21(token: Token) = {
    val iType = semanticStack.popT[Token]()
    var params = ListBuffer[SymbolTableEntry with DataEntry]()
    while (semanticStack.nonEmpty &&
      !semanticStack.head.isInstanceOf[SymbolTableEntry with Params]) {
      val entry = if (isArray) {
        val ub = semanticStack.popT[Token with CONSTANT]().value.toInt
        val lb = semanticStack.popT[Token with CONSTANT]().value.toInt
        val id = semanticStack.popT[IDENTIFIER]()
        ArrayEntry(id.value, localMemory, iType.toString, ub, lb)
      } else {
        val id = semanticStack.popT[IDENTIFIER]()
        VariableEntry(id.value, localMemory, iType.toString)
      }
      symbolTable().insert(entry)
      entry +=: params
      localMemory = localMemory + 1
      parmCount.push(parmCount.pop() + 1)
    }
    val procedure = semanticStack.popT[SymbolTableEntry]()
    val newEntry = globalTable(procedure.name) match {
      case p: ProcedureEntry => p.copy(parameterInfo = (p.parameterInfo ++ params).toList)
      case f: FunctionEntry => f.copy(parameterInfo = (f.parameterInfo ++ params).toList)
    }
    globalTable.insert(newEntry)
    semanticStack.push(newEntry)
    isArray = false
    Success("SA complete")
  }

  def action22(token: Token) = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType != RELATIONAL) {
      Failure(GenericSemanticError("Error at "+ token))
    } else {
      backPatch(semanticStack.headT[List[Int]](), nextQuad)
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
      backPatch(semanticStack.headT[List[Int]](), nextQuad)
      Success("SA complete")
    }
  }

  def action26(token: Token) = {
    val beginLoop = semanticStack.popT[Int]()
    val eFalse = semanticStack.popT[List[Int]]()
    val eTrue = semanticStack.popT[List[Int]]()

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
    println(global)
    val id = token.value
    val result = symbolTable().lookup(id)
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
      val id1 = semanticStack.popT[DataEntry]()
      val offset = semanticStack.pop()
      val eType2 = semanticStack.popT[ETYPE]()
      val id2 = semanticStack.popT[DataEntry]()
      val check = typeCheck(id1, id2)
      if (check == 3) {
        Failure(GenericSemanticError("Error at "+ token))
      } else {
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
        Success("SA complete")
      }
    }
  }

  def action32(token: Token) = {
    val entry = symbolTable().lookup(token.value)
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

  //not complete
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
    val parmInfo = globalTable.lookup(token.value).head match {
      case p: ProcedureEntry => p.parameterInfo
    }
    nextParm.push(parmInfo)
    Success("SA complete")
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
        val id = semanticStack.popT[SymbolTableEntry with DataEntry]()
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
      val id1 = semanticStack.popT[DataEntry]()
      val operator = semanticStack.popT[Token]()
      val id2 = semanticStack.popT[DataEntry]()
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

  //incomplete relational action
  def action44(token: Token) = {
    val eType = semanticStack.popT[ETYPE]()
    if (eType == RELATIONAL && token == mulOps.and) {
      val eTrue = semanticStack.headT[List[Int]]()
      backPatch(eTrue, nextQuad)
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
      semanticStack.push(ARITHMETIC)
    }
    Success("SA complete")
  }

  def action46(token: Token): Try[String] = {
    val id = token.value
    if (token.isInstanceOf[IDENTIFIER]) {
      val result = symbolTable().lookup(id)
      result.map((res) => {
        semanticStack.push(res)
      })
      if (result.isEmpty) return Failure(UndeclaredVariable("Identifier " + id +" used before declaration"))
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
      println(offset.dataType)
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
    while (semanticStack(1).isInstanceOf[VariableEntry]) {
      val eType = semanticStack.pop()
      val id = semanticStack.popT[VariableEntry]()
      localMemory = localMemory + 1
      params += id
    }
    params.foreach(id => gen("param", id.name))
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

  def action51() = {
    val proc = semanticStack.find(_.isInstanceOf[ProcedureEntry]) match {
      case Some(p: ProcedureEntry) => p
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
      params.foreach(id => gen("param", id.name))
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
    var params = new ListBuffer[SymbolTableEntry with Address with DataEntry]()
    while (semanticStack.head.isInstanceOf[SymbolTableEntry with Address with DataEntry]) {
      val id = semanticStack.popT[SymbolTableEntry with Address with DataEntry]()
      params += id
    }
    params.foreach(id => {
      gen("print", "<"+id.name+ "> = ")
      if (id.dataType == DTYPE.real) {
        gen("foutp", id)
      } else {
        gen("outp", id)
      }
      gen("newl")
    })
    val eType = semanticStack.pop()
    val id = semanticStack.pop()
    val pc = parmCount.pop()
    Success("SA complete")
  }

  def action51Read() = {
    var params = new ListBuffer[SymbolTableEntry with Address with DataEntry]()
    while (semanticStack.head.isInstanceOf[SymbolTableEntry with Address with DataEntry]) {
      val id = semanticStack.popT[SymbolTableEntry with Address with DataEntry]()
      params += id
    }
    params.foreach(id => {
      if (id.dataType == DTYPE.real) {
        gen("finp", id)
      } else {
        gen("inp", id)
      }
    })
    val eType = semanticStack.pop()
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
      gen("move", func.result, tmp)
      semanticStack.push(tmp)
      semanticStack.push(ARITHMETIC)
      Success("SA complete")
    }
  }

  def action53(token: Token): Try[String] = {
    val entry = symbolTable().lookup(token.value)
    entry match {
      case Some(e: FunctionEntry) => {
        val eType = semanticStack.popT[ETYPE]()
        val id = semanticStack.popT[SymbolTableEntry]
        if (id.name != currentFunction.name) {
          //non-matching functions
          return Failure(GenericSemanticError("Error at "+ token))
        }
        semanticStack.push(globalTable.lookup(e.result).head)
        semanticStack.push(ARITHMETIC)
        Success("SA complete")
      }
      case _ => Success("SA complete")
    }
  }

  def action54(token: Token) = {
    val entry = symbolTable().lookup(token.value)
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
    action match {
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
      case Action42 => action42(token)
      case Action43 => action43(token)
      case Action44 => action44(token)
      case Action45 => action45(token)
      case Action46 => action46(token)
      case Action47 => action47(token)
      case Action48 => action48(token)
      case Action49 => action49(token)
      case Action50 => action50(token)
      case Action51 => action51()
      case Action52 => action52(token)
      case Action53 => action53(token)
      case Action54 => action54(token)
      case Action55 => action55()
      case Action56 => action56()
      case _ => println("action not yet implemented"); Success("SA complete")
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
      case a: Address => genOffset(a.address)
      case c: ConstantEntry => c.name
      case f: FunctionEntry => globalTable.lookup(f.result).head match {
        case t: Address => genOffset(t.address) }
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
    println("backpatching")
    println("prev " + previousList + "target " + target)
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

  def getTvi(): String = {
    val body = quadruples.tail.foldLeft((0, "")){case ((index, output), quad) => {
      (index + 1, output + "\n" + index + ":   " + quad.toString)
    }}._2
    quadruples.head.toString + body
  }

  def semanticStackDump() = {
    println("Contents of semantic stack")
    semanticStack.foreach(println(_))
  }

}


