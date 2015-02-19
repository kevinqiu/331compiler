package lex

import collection.mutable.Stack
import collection.mutable.ListBuffer
import scalaz._
import scala.util.{Try, Success, Failure}
import CS331.errors._;

class LexicalAnalyzer(var stream: CharStream, var end: Boolean = false) {

  var lastToken: Try[Token] = Failure(new Exception("placehilder"))

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
    isOperator(c) || isSeparator(c) || c.isSpaceChar || c == CharStream.EOF
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

  def returnLast(s: StringBuilder) = {
    val last = s.last
    if (last != ' ') stream.pushBack(last)
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

  def constructNumber(sign: String, digits: String, decimal: String, exponent: String) = {
    val s = if (sign == "-") -1 else 1
    val d = Try(digits.toInt)
    val dec: Try[Double] = Try(decimal.toDouble)
    val e = Try(exponent.tail.toDouble)
    d.map(i => i * s)
      .map(i => i + dec.getOrElse(0.0))
      .map(i => i* Math.pow(10,e.getOrElse(0))).get
  }

  //not finished, working on identifier method first since simpler
  def processNumber(s: StringBuilder) : Try[Token] = {
    val numberRegExp = """([+,-])?(\d+)(.\d+)?([Ee][-+]?\d+)?""".r
    val number = Try(s.toString match {
      case numberRegExp(sign, digits, decimal, exponent) => constructNumber(sign, digits, decimal, exponent)
    })
    number.map(x => if (x.isValidInt) INTCONSTANT(x.toInt) else REALCONSTANT(x))
  }


  def isAddOpLast(last: Try[Token]): Boolean = {
    def examineLast(t: Token) = t match {
      case RIGHTPAREN(x) => true
      case RIGHTBRACKET(x) => true
      case IDENTIFIER(x) => true
      case INTCONSTANT(x) => true
      case REALCONSTANT(x) => true
      case _ => false
    }
    last.filter(examineLast).isSuccess
  }

  def processOps(s: StringBuilder, last: Try[Token]) : Try[Token] = {
    def findOpToken(s: StringBuilder, l: List[String], t: (Int, String) => Token) = {
      val matching = l.find(x => x.equalsIgnoreCase(s.toString))
      matching.map(m => t(l.indexOf(m)+1, m))
    }

    def isAddOp(x: Token) = {
      x match {
        case ADDOP(y, "+") => isAddOpLast(last)
        case ADDOP(y, "-") => isAddOpLast(last)
        case _ => true
      }
    }

    val matches = List((relops, RELOP), (mulops, MULOP), (addops, ADDOP)).map({case (l, f) => findOpToken(s, l, f)}).flatten

    val t = if (matches.nonEmpty) {
      Success(matches.head)
    } else {
      Failure(new Exception("Malformed symbol"))
    }
    t.filter(isAddOp)
  }

  def processSymbols(s: StringBuilder, last: Try[Token]): Try[Token] = {
    def isUnaryOp(x: Token) = {
      x match {
        case UNARYMINUS(y) => !isAddOpLast(last)
        case UNARYPLUS(y) => !isAddOpLast(last)
        case _ => true
      }
    }
    val t = Try(matchKeywordList(symbols, s).head)
    //check previous to see if token is unary or addop
    t.filter(isUnaryOp)
  }

  def eof(s: StringBuilder): Try[Token] = {
    if (s.head == CharStream.EOF && s.length == 1) {
      end = true
      Success(ENDOFFILE())
    } else {
      Failure(new Exception("Not end"))
    }
  }

  def process(s: StringBuilder, last: Try[Token]): List[Try[Token]] = {
    /*s.toString match {
      case r"" => 
    }*/
    List(processOps(s, last), processSymbols(s, last), processIdentifier(s), processNumber(s), eof(s))
  }

  def nextToken() : Try[Token] = {
    var current = new StringBuilder
    current += getChar()
    var possibleToken = process (current, lastToken)
    var currentToken = possibleToken
    while (possibleToken.exists(_.isSuccess) || (!isDelim(current.last) && current.last.isDigit)) {
      current += getChar()
      currentToken = possibleToken
      possibleToken = process (current, lastToken)
    }
    println("current "+current)
    val findSuccess = currentToken.find(_.isSuccess)
    if (findSuccess.nonEmpty) returnLast(current)
    //set lastToken to be the new token for next method call
    lastToken = findSuccess.getOrElse(currentToken.head)
    lastToken
  }

  def getToken(): Try[Token] = {
    Try(nextToken()).flatten
  }
}
