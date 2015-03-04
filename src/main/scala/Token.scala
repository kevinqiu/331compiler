package lex

abstract class Token{}
abstract class Keyword(val value: String) extends Token

abstract class Symbol(value: String) extends Token
//Reserved Keywords
case object PROGRAM extends Keyword("PROGRAM")
case object BEGIN extends Keyword("BEGIN")
case object END extends Keyword("END")
case object VAR extends Keyword("VAR")
case object FUNCTION extends Keyword("FUNCTION")
case object PROCEDURE extends Keyword("PROCEDURE")
case object RESULT extends Keyword("RESULT")
case object INTEGER extends Keyword("INTEGER")
case object REAL extends Keyword("REAL")
case object ARRAYTOKEN extends Keyword("ARRAY")
case object OF extends Keyword("OF")
case object IF extends Keyword("IF")
case object THEN extends Keyword("THEN")
case object ELSE extends Keyword("ELSE")
case object WHILE extends Keyword("WHILE")
case object DO extends Keyword("DO")
case object NOT extends Keyword("NOT")

case class IDENTIFIER(value: String) extends Token {
  override def equals(o: Any) = o match {
    case o: IDENTIFIER => true
    case _ => false
  }
}
case class INTCONSTANT(value: String) extends Token {
  override def equals(o: Any) = o match {
    case o: INTCONSTANT => true
    case _ => false
  }
}
case class REALCONSTANT(value: String) extends Token {
  override def equals(o: Any) = o match {
    case o: REALCONSTANT => true
    case _ => false
  }
}

//Symbols
case class ADDOP(value: Int, symbol: String) extends Token
case class RELOP(value: Int, symbol: String) extends Token
case class MULOP(value: Int, symbol: String) extends Token

object Ops {
  val relops = List("=", "<>", "<", ">","<=",">=")
  val addops = List("+", "-", "OR")
  val mulops = List("*", "/", "DIV", "MOD", "AND")
}

case object ASSIGNOP extends Keyword(":=")
case object COMMA extends Keyword(",")
case object SEMICOLON extends Keyword(";")
case object COLON extends Keyword(":")
case object RIGHTPAREN extends Keyword(")")
case object LEFTPAREN extends Keyword("(")
case object RIGHTBRACKET extends Keyword("]")
case object LEFTBRACKET extends Keyword("[")
case object UNARYMINUS extends Keyword("-")
case object UNARYPLUS extends Keyword("+")
case object DOUBLEDOT extends Keyword("..")
case object ENDMARKER extends Keyword(".")

case object ENDOFFILE extends Token
//placeholder token for testing, remove in production
case object EMPTYTOKEN extends Token
