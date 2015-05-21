package parser

import lex._
import compiler._
import semanticActions._
import collection.mutable.Stack
import util.{Try, Success, Failure}
import collection.mutable.ListBuffer

class Parser(lexicalAnalyzer: LexicalAnalyzer) {
  var stack: Stack[GrammarSymbol] = new Stack() ++ List(Goal, ENDOFFILE)
  //lookup using (terminal index)(nonterminal index)
  val parseTable: Array[Array[Int]] = readParseTable("parsetable-2const.dat")
  var current: Token = lexicalAnalyzer.next().get
  var last: Token = null
  var semanticActions = new SemanticActions

  //() -> List[Try[String]]
  def parse(): String = {
    var results : ListBuffer[Try[String]] = new ListBuffer()
    var valid = true

    while(stack.length > 0 && valid) {
      val result: Try[String] = stack.head match {
        case t: Token => terminalMatch(t)
        case nt: NonTerminal => parseTableLookUp(current, nt)
        case a: SemanticAction => {
          stack.pop()
          semanticActions.execute(a, last) match {
            case s: Success[String] => s
            case Failure(e) => Failure(GenericSemanticError(e.getMessage +
                " on line "+ lexicalAnalyzer.getLineNumber()))
          }
        }
      }
      if (result.isFailure) {
        println("Error Encountered")
        println(result)
        valid = false
      }
    }
    results.toList
    semanticActions.getTvi()
  }

  def terminalMatch(t: Token) = {
    def matchTerminal(t: Token, c: Token) = {
      t match {
        case o: Op => t.isInstanceOf[Op]
        case _ => t == c
      }
    }
    if (matchTerminal(t, current)) {
      last = current
      stack.pop()
      readInput()
      Success("Terminal Matched")
    } else {
      terminalMatchError()
    }
  }

  def terminalMatchError() = {
    val terminal = stack.pop
    //line currently not provided, will be added in future
    val message = terminal + " missing on line " + lexicalAnalyzer.getLineNumber()  + ", term was inserted automatically"
    Failure(new Missing_Term(message))
  }

  def readInput() = {
    current = lexicalAnalyzer.next().get
  }

  def parseTableLookUp(current: Token, top: NonTerminal) = {
    val entry = parseTable(current.index)(top.index)
    entry match {
      case 999 => nonTerminalMatchError()
      case _ if entry < 0 => {
        stack.pop
        Success("NT Matched") }
      case _ => {
        stack.pop
        pushProduction(entry)
        Success("NT Matched") }
    }
  }

  def nonTerminalMatchError() = {
    val nonMatching = current
    val message = "Syntax Error on line " + lexicalAnalyzer.getLineNumber() + " at " + nonMatching + ", evaluation halted"
    stack.clear
    Failure(new Syntax_Error(message))
  }

  //Int -> () : Side effect, pushes RHS onto Parse stack
  def pushProduction(index: Int) = {
   stack.pushAll(RHSTable.rules(index).reverse)
  }

  //Reads parse table at location
  def readParseTable(location: String): Array[Array[Int]] = {
    io.Source.fromFile(location).getLines
      .map(_.split("\\s+"))
      .filter(_.length > 1)
      .map(_.map(_.toInt)).toArray
  }

}

object RHSTable {
  val rules: Vector[List[GrammarSymbol]] = Vector(
    //production 0
    List(),
    //production 1
    List(PROGRAM, IDENTIFIER(), LEFTPAREN, Identifier_list,
      RIGHTPAREN, Action9, SEMICOLON, Declarations, Sub_declarations,
      Action56, Compound_statement, Action55),
    //production 2
    List(IDENTIFIER(),  Action13,  Identifier_list_tail),
    //production 3
    List(COMMA, IDENTIFIER(), Action13, Identifier_list_tail),
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
    List( Identifier_list,  COLON,  Type,  Action3,
      SEMICOLON,  Declaration_list_tail ),
    //production 9
    List( ),
    //production 10
    List( Standard_type ),
    //production 11
    List( Array_type ),
    //production 12
    List( INTEGER,  Action4 ),
    //production 13
    List( REAL,  Action4 ),
    //production 14
    List( Action6,  ARRAYTOKEN,  LEFTBRACKET,  INTCONSTANT(),
      Action7,  DOUBLEDOT,  INTCONSTANT(), Action7,
      RIGHTBRACKET,  OF,  Standard_type ),
    //production 15
    List( Subprogram_declaration,  Sub_declarations ),
    //production 16
    List( ),
    //production 17
    List( Action1,  Subprogram_head,  Declarations,
      Action5,  Compound_statement,  Action11 ),
    //production 18
    List( FUNCTION,  IDENTIFIER(),  Action15,  Arguments,
      COLON,  RESULT,  Standard_type, SEMICOLON,
      Action16 ),
    //production 19
    List( PROCEDURE,  IDENTIFIER(),  Action17,  Arguments,
      SEMICOLON ),
    //production 20
    List( LEFTPAREN,  Action19,  Parameter_list,  RIGHTPAREN,
      Action20 ),
    //production 21
    List( ),
    //production 22
    List( Identifier_list,  COLON,  Type,  Action21,
      Parameter_list_tail ),
    //production 23
    List( SEMICOLON,  Identifier_list,  COLON,  Type,  Action21,
      Parameter_list_tail ),
    //production 24
    List( ),
    //production 25
    List( BEGIN,  Statement_list,  END ),
    //production 26
    List( Statement,  Statement_list_tail ),
    //production 27
    List( SEMICOLON,  Statement,  Statement_list_tail ),
    //production 28
    List( ),
    //production 29
    List( Elementary_statement ),
    //production 30
    List( IF,  Expression,  Action22,  THEN,
      Statement,  Else_clause ),
    //production 31
    List( WHILE,  Action24,  Expression,  Action25,
      DO,  Statement,  Action26 ),
    //production 32
    List( ELSE,  Action27,  Statement,  Action28 ),
    //production 33
    List( Action29 ),
    //production 34
    List( IDENTIFIER(),  Action30,  Es_tail ),
    //production 35
    List( Compound_statement ),
    //production 36
    List( Action53,  Subscript,  ASSIGNOP,  Expression,
      Action31 ),
    //production 37
    List( Action54,  Parameters ),
    //production 38
    List( Action32,  LEFTBRACKET,  Expression,  RIGHTBRACKET,
      Action33 ),
    //production 39
    List( Action34 ),
    //production 40
    List( Action35,  LEFTPAREN,  Expression_list,  RIGHTPAREN,
      Action51 ),
    //production 41
    List( Action36 ),
    //production 42
    List( Expression,  Action37,  Expression_list_tail ),
    //production 43
    List( COMMA,  Expression,  Action37,  Expression_list_tail ),
    //production 44
    List( ),
    //production 45
    List( Simple_expression,  Expression_tail ),
    //production 46
    List( RELOP(),  Action38,  Simple_expression,  Action39 ),
    //production 47
    List( ),
    //production 48
    List( Term,  Simple_expression_tail ),
    //production 49
    List( Sign,  Action40,  Term,  Action41,
      Simple_expression_tail ),
    //production 50
    List( ADDOP(),  Action42,  Term,  Action43,
      Simple_expression_tail ),
    //production 51
    List( ),
    //production 52
    List( Factor,  Term_tail ),
    //production 53
    List( MULOP(),  Action44,  Factor,  Action45,
      Term_tail ),
    //production 54
    List( ),
    //production 55
    List( IDENTIFIER(),  Action46,  Factor_tail ),
    //production 56
    List( Constant,  Action46 ),
    //production 57
    List( LEFTPAREN,  Expression,  RIGHTPAREN ),
    //production 58
    List( NOT,  Factor,  Action47 ),
    //production 59
    List( Actual_parameters ),
    //production 60
    List( Subscript,  Action48 ),
    //production 61
    List( Action49,  LEFTPAREN,  Expression_list,  RIGHTPAREN,  Action50 ),
    //production 62
    List( Action52 ),
    //production 63
    List( UNARYPLUS ),
    //production 64
    List( UNARYMINUS ),
    //production 65
    List( ProgramNT,  ENDMARKER ),
    //production 66
    List( INTCONSTANT() ),
    //production 67
    List( REALCONSTANT() )
  )
}
