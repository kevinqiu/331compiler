package lex

import collection.mutable.Stack
import collection.mutable.ListBuffer
import scalaz._
import scala.util.{Try, Success, Failure}
import CS331.errors._;

class LexicalAnalyzer(var stream: CharStream) {

  abstract class Token{}
  case class INTCONSTANT(value: Int) extends Token
  case class REALCONSTANT(value: Double) extends Token

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

  def getToken() = {
      try {
        val peek = peekChar()
        if (peek.isLetter) {
        } else if (peek.isDigit) {
          processNumber()
        }
      } catch {
        case e: Exception => Failure(e)
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

    def processNumber() : Try[Token] = {
      var token = new StringBuilder

      def processExponent() = {
          token += getChar()
          if (peekChar().isDigit){
            consumeDigits(token)
          } else {
            Failure(new Exception("No digits after exponent"))
          }
      }

      def createToken(token: StringBuilder) = {
        val value = token.toDouble
        if (value.isValidInt) {
          Success(INTCONSTANT(value.toInt))
        } else {
          Success(REALCONSTANT(value))
        }
      }

      def consumeDigits(t: StringBuilder) = {
        while(peekChar().isDigit) {
          t += getChar
        }
      }
      token += getChar
      consumeDigits(token)
      if (peekChar() == '.'){
        if (peekChar(true) == '.') {
          //if two .. are in a row, then .. is separate token
          createToken(token)
        } else if (peekChar().isDigit){
          consumeDigits(token)
          if (peekChar() == 'E' || peekChar() == 'e'){
            processExponent()
          }
          createToken(token)
        } else {
          Failure(new Exception("No digit after ."))
        }
      } else if (peekChar() == 'E' || peekChar() == 'e'){
        processExponent()
        createToken(token)
      } else if (isDelim(peekChar())) {
        createToken(token)
      } else {
        Failure(new Exception("Unexpected symbol found while evaluating constant"))
      }
    }
  }
}
