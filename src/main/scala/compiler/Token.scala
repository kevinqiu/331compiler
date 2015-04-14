package compiler

trait GrammarSymbol

abstract class Token(val value: String, val index: Int) extends GrammarSymbol

abstract class Keyword(value: String, index: Int) extends Token(value, index)

trait Op { val symbol: String }

//Reserved Keywords
case object PROGRAM extends Keyword("PROGRAM", 0)
case object BEGIN extends Keyword("BEGIN", 1)
case object END extends Keyword("END", 2)
case object VAR extends Keyword("VAR", 3)
case object FUNCTION extends Keyword("FUNCTION", 4)
case object PROCEDURE extends Keyword("PROCEDURE", 5)
case object RESULT extends Keyword("RESULT", 6)
case object INTEGER extends Keyword("INTEGER", 7)
case object REAL extends Keyword("REAL", 8)
case object ARRAYTOKEN extends Keyword("ARRAY", 9)
case object OF extends Keyword("OF", 10)
case object IF extends Keyword("IF", 11)
case object THEN extends Keyword("THEN", 12)
case object ELSE extends Keyword("ELSE", 13)
case object WHILE extends Keyword("WHILE", 14)
case object DO extends Keyword("DO", 15)
case object NOT extends Keyword("NOT", 16)

case class IDENTIFIER(override val value: String = "") extends Token(value, 17) {
  override def equals(o: Any) = o match {
    case o: IDENTIFIER => true
    case _ => false
  }
}

trait CONSTANT {def getType: String}

case class INTCONSTANT(override val value: String = "") extends Token(value, 18) with CONSTANT {
  override def equals(o: Any) = o match {
    case o: INTCONSTANT => true
    case _ => false
  }
  def getType = "integer"
}
case class REALCONSTANT(override val value: String = "") extends Token(value, 19) with CONSTANT {
  override def equals(o: Any) = o match {
    case o: REALCONSTANT => true
    case _ => false
  }
  def getType = "real"
}



//Symbols
case class ADDOP(opIdx: Int = 0, symbol: String = "") extends Token(opIdx.toString, 22) with Op {
  override def equals(o: Any) = o match {
    case o: ADDOP => true
    case _ => false
  }
}
case class RELOP(opIdx: Int = 0, symbol: String = "") extends Token(opIdx.toString, 20) with Op {
  override def equals(o: Any) = o match {
    case o: RELOP => true
    case _ => false
  }
}
case class MULOP(opIdx: Int = 0, symbol: String = "") extends Token(opIdx.toString, 21) with Op {
  override def equals(o: Any) = o match {
    case o: MULOP => true
    case _ => false
  }
}

case object ASSIGNOP extends Keyword(":=", 23)
case object COMMA extends Keyword(",", 24)
case object SEMICOLON extends Keyword(";", 25)
case object COLON extends Keyword(":", 26)
case object RIGHTPAREN extends Keyword(")", 27)
case object LEFTPAREN extends Keyword("(", 28)
case object RIGHTBRACKET extends Keyword("]", 29)
case object LEFTBRACKET extends Keyword("[", 30)
case object UNARYMINUS extends Keyword("-", 31)
case object UNARYPLUS extends Keyword("+", 32)
case object DOUBLEDOT extends Keyword("..", 33)
case object ENDMARKER extends Keyword(".", 34)
case object ENDOFFILE extends Keyword("eof", 35)
