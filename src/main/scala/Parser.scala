package parser

import lex._
import scala.collection.mutable.Stack

class Parser(lexicalAnalyzer: LexicalAnalyzer) {
  var stack = new Stack()
  //val parseTable = readParseTable("parsetable-2const.dat")

  def parseNext() = {

  }

}

object RHSTable {
  val rules = Vector(
    //production 0
    List(),
    //production 1
    List(PROGRAM, IDENTIFIER(""), LEFTPAREN, Identifier_list,
      RIGHTPAREN, Action9, SEMICOLON, Declarations, Sub_declarations,
      Action56, Compound_statement, Action55),
    //production 2
    List(IDENTIFIER,  Action13,  Identifier_list_tail),
    //production 3
    List(COMMA, IDENTIFIER, Action13, Identifier_list_tail)/*,
    //production 4
    List( ),
    //production 5
    List( VAR, Action1, Declaration_list, Action2 ),
    //production 6
    List( ),
    //production 7
    List( Identifier_list, COLON, Type, Action3,
      SEMICOLON, Declaration_list_tail ),
    //production 8
    List( NonTerminal.identifier_list,  TokenType.COLON,  NonTerminal.type,  SemanticAction.action3,
      TokenType.SEMICOLON,  NonTerminal.declaration_list_tail ),
    //production 9
    List( ),
    //production 10
    List( NonTerminal.standard_type ),
    //production 11
    List( NonTerminal.array_type ),
    //production 12
    List( TokenType.INTEGER,  SemanticAction.action4 ),
    //production 13
    List( TokenType.REAL,  SemanticAction.action4 ),
    //production 14
    List( SemanticAction.action6,  TokenType.ARRAY,  TokenType.LEFTBRACKET,  TokenType.INTCONSTANT,
      SemanticAction.action7,  TokenType.DOUBLEDOT,  TokenType.INTCONSTANT, SemanticAction.action7,
      TokenType.RIGHTBRACKET,  TokenType.OF,  NonTerminal.standard_type ),
    //production 15
    List( NonTerminal.subprogram_declaration,  NonTerminal.sub_declarations ),
    //production 16
    List( ),
    //production 17
    List( SemanticAction.action1,  NonTerminal.subprogram_head,  NonTerminal.declarations,
      SemanticAction.action5,  NonTerminal.compound_statement,  SemanticAction.action11 ),
    //production 18
    List( TokenType.FUNCTION,  TokenType.IDENTIFIER,  SemanticAction.action15,  NonTerminal.arguments,
      TokenType.COLON,  TokenType.RESULT,  NonTerminal.standard_type, TokenType.SEMICOLON,
      SemanticAction.action16 ),
    //production 19
    List( TokenType.PROCEDURE,  TokenType.IDENTIFIER,  SemanticAction.action17,  NonTerminal.arguments,
      TokenType.SEMICOLON ),
    //production 20
    List( TokenType.LEFTPAREN,  SemanticAction.action19,  NonTerminal.parameter_list,  TokenType.RIGHTPAREN,
      SemanticAction.action20 ),
    //production 21
    List( ),
    //production 22
    List( NonTerminal.identifier_list,  TokenType.COLON,  NonTerminal.type,  SemanticAction.action21,
      NonTerminal.parameter_list_tail ),
    //production 23
    List( TokenType.SEMICOLON,  NonTerminal.identifier_list,  TokenType.COLON,  NonTerminal.type,  SemanticAction.action21,
      NonTerminal.parameter_list_tail ),
    //production 24
    List( ),
    //production 25
    List( TokenType.BEGIN,  NonTerminal.statement_list,  TokenType.END ),
    //production 26
    List( NonTerminal.statement,  NonTerminal.statement_list_tail ),
    //production 27
    List( TokenType.SEMICOLON,  NonTerminal.statement,  NonTerminal.statement_list_tail ),
    //production 28
    List( ),
    //production 29
    List( NonTerminal.elementary_statement ),
    //production 30
    List( TokenType.IF,  NonTerminal.expression,  SemanticAction.action22,  TokenType.THEN,
      NonTerminal.statement,  NonTerminal.else_clause ),
    //production 31
    List( TokenType.WHILE,  SemanticAction.action24,  NonTerminal.expression,  SemanticAction.action25,
      TokenType.DO,  NonTerminal.statement,  SemanticAction.action26 ),
    //production 32
    List( TokenType.ELSE,  SemanticAction.action27,  NonTerminal.statement,  SemanticAction.action28 ),
    //production 33
    List( SemanticAction.action29 ),
    //production 34
    List( TokenType.IDENTIFIER,  SemanticAction.action30,  NonTerminal.es_tail ),
    //production 35
    List( NonTerminal.compound_statement ),
    //production 36
    List( SemanticAction.action53,  NonTerminal.subscript,  TokenType.ASSIGNOP,  NonTerminal.expression,
      SemanticAction.action31 ),
    //production 37
    List( SemanticAction.action54,  NonTerminal.parameters ),
    //production 38
    List( SemanticAction.action32,  TokenType.LEFTBRACKET,  NonTerminal.expression,  TokenType.RIGHTBRACKET,
      SemanticAction.action33 ),
    //production 39
    List( SemanticAction.action34 ),
    //production 40
    List( SemanticAction.action35,  TokenType.LEFTPAREN,  NonTerminal.expression_list,  TokenType.RIGHTPAREN,
      SemanticAction.action51 ),
    //production 41
    List( SemanticAction.action36 ),
    //production 42
    List( NonTerminal.expression,  SemanticAction.action37,  NonTerminal.expression_list_tail ),
    //production 43
    List( TokenType.COMMA,  NonTerminal.expression,  SemanticAction.action37,  NonTerminal.expression_list_tail ),
    //production 44
    List( ),
    //production 45
    List( NonTerminal.simple_expression,  NonTerminal.expression_tail ),
    //production 46
    List( TokenType.RELOP,  SemanticAction.action38,  NonTerminal.simple_expression,  SemanticAction.action39 ),
    //production 47
    List( ),
    //production 48
    List( NonTerminal.term,  NonTerminal.simple_expression_tail ),
    //production 49
    List( NonTerminal.sign,  SemanticAction.action40,  NonTerminal.term,  SemanticAction.action41,
      NonTerminal.simple_expression_tail ),
    //production 50
    List( TokenType.ADDOP,  SemanticAction.action42,  NonTerminal.term,  SemanticAction.action43,
      NonTerminal.simple_expression_tail ),
    //production 51
    List( ),
    //production 52
    List( NonTerminal.factor,  NonTerminal.term_tail ),
    //production 53
    List( TokenType.MULOP,  SemanticAction.action44,  NonTerminal.factor,  SemanticAction.action45,
      NonTerminal.term_tail ),
    //production 54
    List( ),
    //production 55
    List( TokenType.IDENTIFIER,  SemanticAction.action46,  NonTerminal.factor_tail ),
    //production 56
    List( NonTerminal.constant,  SemanticAction.action46 ),
    //production 57
    List( TokenType.LEFTPAREN,  NonTerminal.expression,  TokenType.RIGHTPAREN ),
    //production 58
    List( TokenType.NOT,  NonTerminal.factor,  SemanticAction.action47 ),
    //production 59
    List( NonTerminal.actual_parameters ),
    //production 60
    List( NonTerminal.subscript,  SemanticAction.action48 ),
    //production 61
    List( SemanticAction.action49,  TokenType.LEFTPAREN,  NonTerminal.expression_list,  TokenType.RIGHTPAREN,  SemanticAction.action50 ),
    //production 62
    List( SemanticAction.action52 ),
    //production 63
    List( TokenType.UNARYPLUS ),
    //production 64
    List( TokenType.UNARYMINUS ),
    //production 65
    List( NonTerminal.program,  TokenType.ENDMARKER ),
    //production 66
    List( TokenType.INTCONSTANT ),
    //production 67
    List( TokenType.REALCONSTANT )*/
  )
}
