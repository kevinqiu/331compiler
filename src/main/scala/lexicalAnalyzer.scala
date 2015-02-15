package lex

import collection.mutable.Stack
import collection.mutable.ListBuffer
import scalaz._
import scala.util.{Try, Success, Failure}
import CS331.errors._;

class LexicalAnalyzer(var stream: CharStream, var end: Boolean = false) {

  val keywords = List(PROGRAM(), BEGIN(), END(), VAR(), FUNCTION(), PROCEDURE(), RESULT(), INTEGER(), REAL(), ARRAYTOKEN(), OF(), IF(), THEN(), ELSE(), WHILE(), DO(), NOT())

  val relops = List("=", "<>", "<", ">","<=",">=")
  val addops = List("+", "-", "OR")
  val mulops = List("*", "/", "DIV", "MOD", "AND")
  val symbols = List(ASSIGNOP(), COMMA(), SEMICOLON(), COLON(), RIGHTPAREN(), LEFTPAREN(), RIGHTBRACKET(), LEFTBRACKET(), UNARYMINUS(), UNARYPLUS(), DOUBLEDOT(), ENDMARKER())

  def isPeriod(c: Char) = {
    c == '.'
  }

  def isOperator(c: Char) = {
    "+-*/".contains(c)
  }

  def isSeparator(c: Char) = {
    ";,()".contains(c)
  }

  def isDelim(c: Char) = {
    isOperator(c) || isSeparator(c) || c.isSpaceChar
  }

  def matchKeywordList(l: List[Token with Keyword], s: StringBuilder): Option[Token] = {
    l.find(x => x.value.equalsIgnoreCase(s.toString))
  }

  def peekChar(twoCharLookahead:Boolean = false) = {
    val c = stream.currentChar
    if (twoCharLookahead) {
      val d = stream.currentChar
      stream.pushBack(c)
      stream.pushBack(d)
      d
    } else {
      stream.pushBack(c)
      c
    }
  }

  def getChar() = {
    stream.currentChar
  }

  def processIdentifier(s: StringBuilder) : Try[Token] = {
    if (s.head.isLetter) {
      if(s.forall{ x => x.isLetterOrDigit }) {
        val matchingKeyword: Option[Token] = matchKeywordList(keywords, s)
        Success(matchingKeyword.getOrElse(IDENTIFIER(s.toString)))
      } else {
        Failure(new Exception("Idenifier has non alphanumeric characters"))
      }
    }
    else {
      Failure(new Exception("Idenifier doesn't start with letter"))
    }
  }

  //not finished, working on identifier method first since simpler
  def processNumber(s: StringBuilder) : Try[Token] = {
    val number = Try(s.toDouble)
    number.map(x => if (x.isValidInt) INTCONSTANT(x.toInt) else REALCONSTANT(x))
  }

  def processOps(s: StringBuilder) : Try[Token] = {
    def findOpToken(s: StringBuilder, l: List[String], t: (Int, String) => Token) = {
      val matching = l.find(x => x.equalsIgnoreCase(s.toString))
      matching.map(m => t(l.indexOf(m)+1, m))
    }

    val matches = List((relops, RELOP), (mulops, MULOP), (addops, ADDOP)).map({case (l, f) => findOpToken(s, l, f)}).flatten

    if (matches.nonEmpty) {
      Success(matches.head)
    } else {
      Failure(new Exception("Malformed symbol"))
    }
  }

  def processSymbols(s: StringBuilder): Try[Token] = {
    Try(matchKeywordList(symbols, s).head)
  }

  def eof(s: StringBuilder): Try[Token] = {
    if (s.head == CharStream.EOF && s.length == 1) {
      end = true
      Success(ENDOFFILE())
    } else {
      Failure(new Exception("Not end"))
    }
  }

  def process(s: StringBuilder): List[Try[Token]] = {
    List(processIdentifier(s), processNumber(s), processOps(s), processSymbols(s), eof(s))
  }

  def nextToken() : Try[Token] = {
    val peek = peekChar()
    var current = new StringBuilder
    current += getChar()
    var possibleToken = process (current)
    var currentToken = possibleToken
    while (possibleToken.exists(_.isSuccess)) {
      current += getChar()
      currentToken = possibleToken
      possibleToken = process (current)
    }
    currentToken.find(_.isSuccess).getOrElse(currentToken.head)
  }

  def getToken(): Try[Token] = {
    Try(nextToken()).flatten
  }
}
