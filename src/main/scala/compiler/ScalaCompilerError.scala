package compiler

abstract class CompilerError(message: String) extends Exception(message)

abstract class ParserError(message: String) extends CompilerError(message)

case class Missing_Term(message: String) extends ParserError(message)
case class Syntax_Error(message: String) extends ParserError(message)

abstract class LexicalError(message: String) extends CompilerError(message)
case class Identifier_Too_Long(message: String) extends LexicalError(message)

abstract class SemanticError(message: String) extends CompilerError(message)
case class GenericSemanticError(message: String) extends SemanticError(message)
case class UndeclaredVariable(message: String) extends SemanticError(message)
