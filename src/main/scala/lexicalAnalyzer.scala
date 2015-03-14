package lex

import collection.mutable.Stack
import collection.mutable.ListBuffer
import scalaz._
import scala.util.{Try, Success, Failure}
import compiler._
import CS331.errors._;

//end indicates when ENDMARKER has appeared and nextToken results in no new values
class LexicalAnalyzer(var stream: CharStream, var end: Boolean = false) {

  var lastToken: Try[Token] = Failure(new Exception("placeholder"))

  //categorize Tokens
  val keywords = List(PROGRAM, BEGIN, END, VAR, FUNCTION, PROCEDURE, RESULT, INTEGER, REAL, ARRAYTOKEN, OF, IF, THEN, ELSE, WHILE, DO, NOT)

  val symbols = List(ASSIGNOP, COMMA, SEMICOLON, COLON, RIGHTPAREN, LEFTPAREN, RIGHTBRACKET, LEFTBRACKET, UNARYMINUS, UNARYPLUS, DOUBLEDOT, ENDMARKER)

  val opSymbol = List(RELOP(1, "="), RELOP(2, "<>"), RELOP(3, "<"), RELOP(4, ">"), RELOP(5,"<="),RELOP(6, ">="), ADDOP(1, "+"), ADDOP(2, "-"), MULOP(1, "*"), MULOP(2, "/"))

  val opString = List(MULOP(3, "div"), MULOP(4, "mod"), MULOP(5, "and"),ADDOP(3, "or"))


  //merge both of these functions and combine Op trait with Keyword
  private def matchKeywordList(l: List[Token with Keyword], s: StringBuilder): Option[Token] = {
    l.find(x => x.value.equalsIgnoreCase(s.toString))
  }

  private def matchOp(l: List[Token with Op], s: StringBuilder): Option[Token] = {
    l.find(x => x.symbol.equalsIgnoreCase(s.toString))
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

  //Searches input for tokens that are strings
  private def processIdentifier(s: StringBuilder) : Try[Token] = {
    if (s.head.isLetter) {
      if(s.forall{ x => x.isLetterOrDigit }) {

        val matches = List(matchOp(opString, s), matchKeywordList(keywords, s)).flatten
        Success(matches.headOption.getOrElse(IDENTIFIER(s.toString)))
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

  //Searches input for Op Tokens
  private def processOps(s: StringBuilder) : Try[Token] = {

    def isAddOp(x: Token) = {
      x match {
        case ADDOP(y, "+") => isAddOpLast()
        case ADDOP(y, "-") => isAddOpLast()
        case _ => true
      }
    }

    val opMatch = matchOp(opSymbol, s)

    val t = if (opMatch.nonEmpty) {
      Success(opMatch.head)
    } else {
      Failure(new Exception("Malformed Op: "+s.toString))
    }
    t.filter(isAddOp)
  }

  //Searches Input for non-alphanumeric Tokens
  private def processSymbols(s: StringBuilder): Try[Token] = {
    def isUnaryOp(x: Token) = {
      x match {
        case UNARYMINUS => !isAddOpLast()
        case UNARYPLUS => !isAddOpLast()
        case _ => true
      }
    }
    val t = Try(matchKeywordList(symbols, s).head)
    //check previous to see if token is unary or addop
    t.filter(isUnaryOp)
  }

  //Searches Input for EOF Marker
  private def eof(s: StringBuilder): Try[Token] = {
    if (s.head == CharStream.EOF && s.length == 1) {
      end = true
      Success(ENDOFFILE)
    } else {
      Failure(new Exception("Malformed symbol: "+ s.toString))
    }
  }

  //wrapper to search chars, fails when no matches are found on additional chars
  private def process(f: (StringBuilder) => Try[Token]): (Try[Token], Int) = {
    var buffer = new StringBuilder
    if (peekChar() == ' ') { getChar(); buffer += getChar() } else { buffer += getChar() }
    var currentEval = f(buffer)
    var possibleEval = currentEval
    while (possibleEval.isSuccess) {
      buffer += getChar()
      currentEval = possibleEval
      possibleEval = f(buffer)
    }
    pushBackString(buffer)
    val length = buffer.length-1
    (currentEval, length)
  }


  private def nextToken() : Try[Token] = {
    //list of results sorted by length of match
    //remember that order is important
    val results: List[(Try[Token], Int)] = List(process(processIdentifier), process(eof), process(processSymbols), processForNum(), process(processOps)).sortBy(x => x._2).reverse
    val findSuccess = results.filter(_._1.isSuccess)
    if (findSuccess.nonEmpty) {
      val tokenInt = findSuccess.head
      takeChars(tokenInt._2)
      lastToken = tokenInt._1
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
