package lex

abstract class Token{}
trait Keyword{val value: String}
abstract class Symbol(value: String) extends Token
//Reserved Keywords
case class PROGRAM(value:String = "PROGRAM") extends Token with Keyword
case class BEGIN(value:String = "BEGIN") extends Token with Keyword
case class END(value:String = "END") extends Token with Keyword
case class VAR(value:String = "VAR") extends Token with Keyword
case class FUNCTION(value:String = "FUNCTION") extends Token with Keyword
case class PROCEDURE(value:String = "PROCEDURE") extends Token with Keyword
case class RESULT(value:String = "RESULT") extends Token with Keyword
case class INTEGER(value:String = "INTEGER") extends Token with Keyword
case class REAL(value:String = "REAL") extends Token with Keyword
case class ARRAYTOKEN(value:String = "ARRAY") extends Token with Keyword
case class OF(value:String = "OF") extends Token with Keyword
case class IF(value:String = "IF") extends Token with Keyword
case class THEN(value:String = "THEN") extends Token with Keyword
case class ELSE(value:String = "ELSE") extends Token with Keyword
case class WHILE(value:String = "WHILE") extends Token with Keyword
case class DO(value:String = "DO") extends Token with Keyword
case class NOT(value:String = "NOT") extends Token with Keyword

case class IDENTIFIER(value: String) extends Token
case class INTCONSTANT(value: String) extends Token
case class REALCONSTANT(value: String) extends Token

//Symbols
case class ADDOP(value: Int, symbol: String) extends Token
case class RELOP(value: Int, symbol: String) extends Token
case class MULOP(value: Int, symbol: String) extends Token

case class ASSIGNOP(value:String = ":=") extends Token with Keyword
case class COMMA(value:String = ",") extends Token with Keyword
case class SEMICOLON(value:String = ";") extends Token with Keyword
case class COLON(value:String = ":") extends Token with Keyword
case class RIGHTPAREN(value:String = ")") extends Token with Keyword
case class LEFTPAREN(value:String = "(") extends Token with Keyword
case class RIGHTBRACKET(value:String = "]") extends Token with Keyword
case class LEFTBRACKET(value:String = "[") extends Token with Keyword
case class UNARYMINUS(value:String = "-") extends Token with Keyword
case class UNARYPLUS(value:String = "+") extends Token with Keyword
case class DOUBLEDOT(value:String = "..") extends Token with Keyword
case class ENDMARKER(value:String = ".") extends Token with Keyword

case class ENDOFFILE() extends Token
//placeholder token for testing, remove in production
case class EMPTYTOKEN() extends Token
