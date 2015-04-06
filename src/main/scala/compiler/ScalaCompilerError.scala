package compiler

abstract class ParserError(message: String)

case class Missing_Term(message: String) extends Exception(message)
case class Syntax_Error(message: String) extends Exception(message)
case class Identifier_Too_Long(message: String) extends Exception(message)
