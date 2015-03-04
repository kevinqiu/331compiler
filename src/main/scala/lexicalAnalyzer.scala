package lex

import collection.mutable.Stack
import collection.mutable.ListBuffer
import scalaz._
import scala.util.{Try, Success, Failure}
import CS331.errors._;

//end indicates when ENDMARKER has appeared and nextToken results in no new values
class LexicalAnalyzer(var stream: CharStream, var end: Boolean = false) {

  var lastToken: Try[Token] = Failure(new Exception("placeholder"))

  //categorize Tokens
  val keywords = List(PROGRAM, BEGIN, END, VAR, FUNCTION, PROCEDURE, RESULT, INTEGER, REAL, ARRAYTOKEN, OF, IF, THEN, ELSE, WHILE, DO, NOT)

  val symbols = List(ASSIGNOP, COMMA, SEMICOLON, COLON, RIGHTPAREN, LEFTPAREN, RIGHTBRACKET, LEFTBRACKET, UNARYMINUS, UNARYPLUS, DOUBLEDOT, ENDMARKER)

  private def matchKeywordList(l: List[Token with Keyword], s: StringBuilder): Option[Token] = {
    l.find(x => x.value.equalsIgnoreCase(s.toString))
  }

  //non-destructively lookahead
  private def peekChar(twoCharLookahead:Boolean = false) = {
    val c = getChar()
    if (twoCharLookahead) {
      val d = getChar()
      pushBackChar(c)
      pushBackChar(d)
      d
    } else {
      pushBackChar(c)
      c
    }
  }

  //consume chars
  private def takeChars(n: Int) = {
    for (i <- 0 until n){
      getChar()
    }
  }

  private def getChar() = {
    stream.currentChar
  }

  private def pushBackString(b: StringBuilder) = {
    b.reverse.foreach(x => pushBackChar(x))
  }

  private def pushBackChar(c: Char) = {
    stream.pushBack(c)
  }

  //Searches input for Identifier or Keyword tokens
  private def processIdentifier(s: StringBuilder, last: Try[Token]) : Try[Token] = {
    if (s.head.isLetter) {
      if(s.forall{ x => x.isLetterOrDigit }) {
        val matchingKeyword: Option[Token] = matchKeywordList(keywords, s)
        Success(matchingKeyword.getOrElse(IDENTIFIER(s.toString)))
      } else {
        Failure(new Exception("Idenifier has non alphanumeric characters"))
      }
    }
    else {
      Failure(new Exception("Idenifier doesn't start with letter: "+s.toString))
    }
  }

  private def constructNumber(digits: String, decimal: String, exponent: String) = {
    val d = Try(digits.toInt)
    //scala regular expressions are being weird so check if exponent and decimal
    //are assigned to correct variables
    var decPoss = Option(decimal)
    var ePoss = Option(exponent)
    //if exponent is assigned to decimal, resassgn to exponent
    decPoss.map(x => if (x.head == 'e' || x.head == 'E') {ePoss = decPoss; decPoss = Option(exponent)})
    val dec = decPoss.map(x => x.toDouble)
    val e = ePoss.map(x => x.tail.toDouble)
    d.map(i => i + dec.getOrElse(0.0))
      .map(i => i* Math.pow(10,e.getOrElse(0))).get
  }

  //Searches input for Numbers - similar to generic process function
  //Is different from generic function because of the nature of parsing
  //numbers, needs to be fixed so that generic can be used with numbers perhaps
  private def processForNum() : (Try[Token], Int) = {
    if (peekChar() == ' ') getChar()
    def isValidNum(c: Char) = {
      c.isDigit || c == '.' || c.toLower == 'e'
    }
    var s = new StringBuilder
    var success: Try[Token] = Failure(new Exception("number doesn't begin with sign or number: "+s.toString));
    var length = 0
    while (isValidNum(peekChar())){
      s += getChar()
      val possible = processNumber(s)
      if (possible.isSuccess) {
        length = s.length
        success = possible
      }
    }
    //Special case when constant overflows Java primitives and Pascal primitive
    if (s.forall( x => x.isDigit) && s.length != length) {
      success = Failure(new Exception("Constant too large"))
      length = s.length
    }
    pushBackString(s)
    (success, length)
  }

  private def processNumber(s: StringBuilder) : Try[Token] = {
    val numberRegExp = new util.matching.Regex("""(\d+)(.\d+)?([Ee][-+]?\d+)?""", "sign", "digits", "decimal", "exponent")
    val number = Try(s.toString match {
          //scala regexp working strangely, somtimes 3rd capture group is shifted to second
      case numberRegExp(digits, decimal, exponent) => constructNumber(digits, decimal, exponent)
    })
    number.map(x => {
      if (x.isValidInt) { INTCONSTANT(x.toString) }
      //Java primitives are less than the max of Pascal
      else if (!x.isInfinite) { REALCONSTANT(x.toString) }
      else { REALCONSTANT(s.toString) }})
  }

  //determines if the current token is an ADDOP based on last token
  private def isAddOpLast(last: Try[Token]): Boolean = {
    def examineLast(t: Token) = t match {
      case RIGHTPAREN => true
      case RIGHTBRACKET => true
      case IDENTIFIER(y) => true
      case INTCONSTANT(y) => true
      case REALCONSTANT(y) => true
      case _ => false
    }
    last.filter(examineLast).isSuccess
  }

  //Searches input for Op Tokens
  private def processOps(s: StringBuilder, last: Try[Token]) : Try[Token] = {
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

    val matches = List((Ops.relops, RELOP), (Ops.mulops, MULOP), (Ops.addops, ADDOP)).map({case (l, f) => findOpToken(s, l, f)}).flatten

    val t = if (matches.nonEmpty) {
      Success(matches.head)
    } else {
      Failure(new Exception("Malformed symbol: "+s.toString))
    }
    t.filter(isAddOp)
  }

  //Searches Input for non-alphanumeric Tokens
  private def processSymbols(s: StringBuilder, last: Try[Token]): Try[Token] = {
    def isUnaryOp(x: Token) = {
      x match {
        case UNARYMINUS => !isAddOpLast(last)
        case UNARYPLUS => !isAddOpLast(last)
        case _ => true
      }
    }
    val t = Try(matchKeywordList(symbols, s).head)
    //check previous to see if token is unary or addop
    t.filter(isUnaryOp)
  }

  //Searches Input for EOF Marker
  private def eof(s: StringBuilder, last: Try[Token]): Try[Token] = {
    if (s.head == CharStream.EOF && s.length == 1) {
      end = true
      Success(ENDOFFILE)
    } else {
      Failure(new Exception("Malformed symbol: "+ s.toString))
    }
  }

  //wrapper to search chars, fails when no matches are found on additional chars
  private def process(f: (StringBuilder, Try[Token]) => Try[Token]): (Try[Token], Int) = {
    var buffer = new StringBuilder
    if (peekChar() == ' ') { getChar(); buffer += getChar() } else { buffer += getChar() }
    var currentEval = f(buffer, lastToken)
    var possibleEval = currentEval
    while (possibleEval.isSuccess) {
      buffer += getChar()
      currentEval = possibleEval
      possibleEval = f(buffer, lastToken)
    }
    pushBackString(buffer)
    val length = buffer.length-1
    (currentEval, length)
  }


  private def nextToken() : Try[Token] = {
    //list of results sorted by length of match
    val results: List[(Try[Token], Int)] = List(process(eof), process(processSymbols), process(processIdentifier), processForNum(), process(processOps)).sortBy(x => x._2).reverse

    val findSuccess = results.filter(_._1.isSuccess)

    if (findSuccess.nonEmpty) {
      takeChars(findSuccess.head._2)
      lastToken = findSuccess.head._1
    } else {
      takeChars(results.head._2)
      lastToken = results.head._1
    }
    lastToken
  }

  //get next token
  def getToken(): Try[Token] = {
    Try(nextToken()).flatten
  }
}
