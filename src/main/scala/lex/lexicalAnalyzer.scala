package lex

import collection.mutable.Stack
import collection.mutable.ListBuffer
import scalaz._
import scala.util.{Try, Success, Failure}
import compiler._
import CS331.errors._;

//end indicates when ENDMARKER has appeared and nextToken results in no new values
class LexicalAnalyzer(var stream: CharStream) extends Iterator[Try[Token]] {

  var lastToken: Try[Token] = Failure(new Exception("placeholder"))
  var end: Boolean = false

  //categorize Tokens
  val keywords = List(PROGRAM, BEGIN, END, VAR, FUNCTION, PROCEDURE, RESULT, INTEGER, REAL, ARRAYTOKEN, OF, IF, THEN, ELSE, WHILE, DO, NOT)

  val opString = List(MULOP(3, "div"), MULOP(4, "mod"), MULOP(5, "and"),ADDOP(3, "or"))

  val simpleOpSymbol = List(RELOP(2, "<>"), RELOP(3, "<"), RELOP(4, ">"), RELOP(5,"<="),RELOP(6, ">="))

  val simpleSymbols = List(COMMA, SEMICOLON, RIGHTPAREN, LEFTPAREN, RIGHTBRACKET, LEFTBRACKET, ASSIGNOP)


  //merge both of these functions and combine Op trait with Keyword
  def matchKeywordList(l: List[Token with Keyword], s: StringBuilder): Option[Token] = {
    l.find(x => x.value.equalsIgnoreCase(s.toString))
  }

  def matchOp(l: List[Token with Op], s: StringBuilder): Option[Token] = {
    l.find(x => x.symbol.equalsIgnoreCase(s.toString))
  }

  def getChar() = {
    stream.currentChar
  }

  def peek = {
    val peek = getChar()
    pushBackChar(peek)
    peek
  }

  def peekTwo = {
    val peek = getChar()
    val peek2 = getChar()
    pushBackChar(peek2)
    pushBackChar(peek)
    peek2
  }

  def pushBackChar(c: Char) = {
    stream.pushBack(c)
  }

  def readIdentifier(s: StringBuilder) : Try[Token] = {
    while (peek.isDigit || peek.isLetter) {
      s += getChar()
    }
    val possibleKey = matchKeywordList(keywords, s)
    val possibleOp = matchOp(opString, s)
    val poss = List(possibleKey, possibleOp).flatten
    if (poss.nonEmpty) {
      Success(poss.head)
    } else {
      val id = s.toString
      if (id.length > 64) {
        Failure(Identifier_Too_Long(id + " exceedes maximum length for identifier names"))
      }
      else Success(IDENTIFIER(s.toString))
    }
  }

  def readNumber(s: StringBuilder): Try[Token] = {

    while (peek.isDigit) {
      s += getChar()
    }

    var realConstant = false

    if (peek == '.') {
      if (!peekTwo.isDigit) {
        return Success(INTCONSTANT(s.toString))
      } else {
        realConstant = true

        //add . to string
        s += getChar()

        while(peek.isDigit) {
          s += getChar()
        }
      }
    }

    if (peek.toLower == 'e') {
      val tmp = getChar()
      if (peek == '+' || peek == '-') {
        if (!peekTwo.isDigit){
          pushBackChar(tmp)
        } else {
          s += tmp
          s += getChar()
          s += getChar()
          realConstant = true
        }
      } else if (peek.isDigit) {
        s += tmp
        s += getChar()
        realConstant = true
      } else {
        pushBackChar(tmp)
      }

      if (realConstant) {
        while (peek.isDigit) {
          s += getChar()
        }
      }
    }

    if (realConstant) {
      Success(REALCONSTANT(s.toString))
    } else {
      Success(INTCONSTANT(s.toString))
    }

  }


  def readSymbol(s: StringBuilder): Try[Token] = {
    s.head match {
      case CharStream.EOF => {
        end = true
        Success(ENDOFFILE)
      }
      case '.' => {
        if (peek == '.') {
          s += getChar()
          Success(DOUBLEDOT)
        } else {
          Success(ENDMARKER)
        }
      }
      case '<' => {
        if (peek == '>') {
          s += getChar()
          Success(RELOP(2, "<>"))
        } else if (peek == '=') {
          s += getChar()
          Success(RELOP(5, "<="))
        } else {
          Success(RELOP(3 , "<"))
        }
      }
      case '>' => {
        if (peek == '=') {
          s += getChar()
          Success(RELOP(6, ">="))
        } else {
          Success(RELOP(4, ">"))
        }
      }
      case ':' => {
        if (peek == '=') {
          s += getChar()
          Success(ASSIGNOP)
        } else {
          Success(COLON)
        }
      }
      case '+' => {
        if (isAddOpLast()) {
          Success(ADDOP(1, "+"))
        } else {
          Success(UNARYPLUS)
        }
      }
      case '-' => {
         if (isAddOpLast()) {
          Success(ADDOP(2, "-"))
        } else {
          Success(UNARYMINUS)
        }
      }
      case '=' => {
        Success(RELOP(1, "="))
      }
      case '*' => {
        Success(MULOP(1, "*"))
      }
      case '/' => {
        Success(MULOP(2, "/"))
      }
      case _ => {
        Try(matchKeywordList(simpleSymbols, s).head)
      }
    }
  }

  //determines if the current token is an ADDOP based on last token
  private def isAddOpLast(): Boolean = {
    def examineLast(t: Token) = t match {
      case RIGHTPAREN => true
      case RIGHTBRACKET => true
      case IDENTIFIER(y) => true
      case INTCONSTANT(y) => true
      case REALCONSTANT(y) => true
      case _ => false
    }
    lastToken.filter(examineLast).isSuccess
  }

  private def nextToken() : Try[Token] = {
    var c = getChar()
    if (c == ' ') {
      c = getChar()
    }
    var s = new StringBuilder(c.toString)
    var changed = false
    if (c.isLetter) {
      lastToken = readIdentifier(s)
    } else if (c.isDigit) {
      lastToken = readNumber(s)
    } else {
      lastToken = readSymbol(s)
    }
    lastToken
  }

  def next(): Try[Token] = {
    Try(nextToken()).flatten
  }

  def hasNext = !end

  def getLineNumber(): Int = {
    stream.lineNumber()
  }

}
