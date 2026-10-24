// CW 2
//======



// Rexps
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 

// new regular expressions
case class RANGE(s: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPTIONAL(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp 
case class RECD(x: String, r: Rexp) extends Rexp


// values - you might have to extend them 
// according to which values you want to create
// for the new regular expressions
abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Rec(x: String, v: Val) extends Val


// convenience for typing regular expressions
import scala.language.implicitConversions

def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

given Conversion[String, Rexp] = (s => charlist2rexp(s.toList))

extension (s: String) {
  def $ (r: Rexp) = RECD(s, r)
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}

extension (r: Rexp) {
  def ~ (s: Rexp) = SEQ(r, s)
  def % = STAR(r)
  def | (s: Rexp) = ALT(r, s)
}




// nullable 
def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true

  case PLUS (r)=> nullable(r)
  case OPTIONAL(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  case RANGE(_) => false
  case RECD(_, r1) => nullable(r)



}

// der 
def der(c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r) => SEQ(der(c, r), STAR(r))
  
  case PLUS(r1) => SEQ(der(c, r1), ALT(PLUS(r1), ONE))
  case RANGE(cs) => if (cs.contains(c)) ONE else ZERO
  case NTIMES(r1, i) => 
      if (i == 0) ZERO else SEQ(der(c, r1), NTIMES(r1, i - 1))
  case OPTIONAL(r1) => der(c,r1)
  case RECD(_, r1) => der(c, r1)


}

// flatten (needs to work with all values) 
def flatten(v: Val) : String = v match {
  case Empty => ""
  case Chr(c) => c.toString
  case Left(v) => flatten(v)
  case Right(v) => flatten(v)
  case Sequ(v1, v2) => flatten(v1) + flatten(v2)
  case Stars(vs) => vs.map(flatten).mkString
}

// env (needs to work with all values) 
def env(v: Val) : List[(String, String)] = v match {
  case Empty => Nil
  case Chr(c) => Nil
  case Left(v) => env(v)
  case Right(v) => env(v)
  case Sequ(v1, v2) => env(v1) ::: env(v2)
  case Stars(vs) => vs.flatMap(env)
  case Rec(x, v1) => (x, flatten(v1)) :: env(v1)
}

// mkeps (needs to be extended for new regular expressions)
def mkeps(r: Rexp) : Val = r match {
  case ONE => Empty
  case ALT(r1, r2) => 
    if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
  case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
  case STAR(r) => Stars(Nil)

  case PLUS(r1) =>  Sequ(mkeps(r1), Stars(Nil))
  case RANGE(cs) => Empty
  case NTIMES(r1, i) => if (i==0) Empty else Sequ(mkeps(r1), Stars(Nil))
  case OPTIONAL(r1) => Right(Empty)
  case RECD(x, r) => Rec(x, mkeps(r))

}

// inj (needs to be extended for new regular expressions)
def inj(r: Rexp, c: Char, v: Val): Val = (r, v) match {
  
  // STAR: append to existing Stars or start a new list
  case (STAR(r1), Sequ(v1, Stars(vs))) => Stars(inj(r1, c, v1) :: vs)
  case (STAR(r1), v1) => Stars(List(inj(r1, c, v1)))
  
  // SEQ
  case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
  
  // ALT
  case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
  case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
  
  // CHAR
  case (CHAR(d), Empty) => Chr(c)
  
  // PLUS: treat as SEQ of r and STAR(r)
  case (PLUS(r1), Sequ(v1, Stars(vs))) => Sequ(inj(r1, c, v1), Stars(vs))
  
  // RANGE
  case (RANGE(cs), Empty) => Chr(c)
  
  // NTIMES
  case (NTIMES(r1, i), Sequ(v1, v2)) if i > 0 => Sequ(inj(r1, c, v1), v2)
  
  // OPTIONAL
  case (OPTIONAL(r1), Left(v1)) => Left(inj(r1, c, v1))
  case (OPTIONAL(r1), Right(v2)) => Right(inj(r1, c, v2))
  
  // RECD
  case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))
  
  // fallback: handle empty STAR/SEQ/ALT combinations gracefully
  case (r1, v1) => v1
}



// the simplification and rectification part
// can be left untouched

// rectification functions
def F_ID(v: Val): Val = v
def F_RIGHT(f: Val => Val) = (v:Val) => Right(f(v))
def F_LEFT(f: Val => Val) = (v:Val) => Left(f(v))
def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Right(v) => Right(f2(v))
  case Left(v) => Left(f1(v))
}
def F_SEQ(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
}
def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) = 
  (v:Val) => Sequ(f1(Empty), f2(v))
def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) = 
  (v:Val) => Sequ(f1(v), f2(Empty))
def F_RECD(f: Val => Val) = (v:Val) => v match {
  case Rec(x, v) => Rec(x, f(v))
}
def F_ERROR(v: Val): Val = throw new Exception("error")

// simp
def simp(r: Rexp): (Rexp, Val => Val) = r match {
  case ALT(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (r2s, F_RIGHT(f2s))
      case (_, ZERO) => (r1s, F_LEFT(f1s))
      case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
                else (ALT (r1s, r2s), F_ALT(f1s, f2s)) 
    }
  }
  case SEQ(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (ZERO, F_ERROR)
      case (_, ZERO) => (ZERO, F_ERROR)
      case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
      case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
      case _ => (SEQ(r1s,r2s), F_SEQ(f1s, f2s))
    }
  }
  case r => (r, F_ID) // this part handles all new regular expressions
}

// lexing generating a value
def lex_simp(r: Rexp, s: List[Char]) : Val = s match {
  case Nil => if (nullable(r)) mkeps(r) else 
    { throw new Exception("lexing error") } 
  case c::cs => {
    val (r_simp, f_simp) = simp(der(c, r))
    inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }
}

// lexing extracting a list of String-String pairs 
def lexing_simp(r: Rexp, s: String) : List[(String, String)] = 
  env(lex_simp(r, s.toList))


// Language specific code for the While Language 
// (you need to create the regular expressions - see CW1) 

//helper char sets
val digits = ('0' to '9').toSet
val non_zero = ('1' to '9').toSet
val lower  = ('a' to 'z').toSet
val upper  = ('A' to 'Z').toSet
val letters = lower ++ upper


val KEYWORD : Rexp = "skip" | "while" | "do" | "if" | "then" | "else" | "read" | "write" | "true" | "false" |"for"| "upto"
val OP: Rexp = ":=" | "=" |"=="| "-" | "+" | "*" | "/" | "%" | "!=" | "<" | ">" | ">=" | "<=" | "&&" | "||"
val LET: Rexp = RANGE(letters)
val SYM : Rexp = LET | RANGE(Set('.', '_', '>', '<', '=', ';', ',','\\',':'))
val PARENS : Rexp = "(" | "{" | ")" | "}"
val DIGIT : Rexp = RANGE(digits)
val SEMI : Rexp = ";"
val WHITESPACE : Rexp =  (" " | "\n" | "\t" | "\r").%
val ID : Rexp = LET ~ ("_" | LET | DIGIT).% 
val NUMBERS: Rexp = "0" | (RANGE(non_zero).% ~ DIGIT.%)
val STRING: Rexp = "\"" ~ (SYM | PARENS | WHITESPACE | "\n").% ~ "\""
val COMMENT: Rexp = "//" ~ (SYM  | " " | PARENS | DIGIT).% ~ "\n".%
val EOL : Rexp = "\n" | ("\r" ~ "\n")

val WHILE_REGS = (("k" .$ (KEYWORD)) | 
                  ("o" .$ (OP)) | 
                  ("str" .$ (STRING)) |
                  ("p" .$ (PARENS)) |
                  ("s" .$ (SEMI)) | 
                  ("w" .$ (WHITESPACE)) | 
                  ("i" .$ (ID)) | 
                  ("n" .$ (NUMBERS)) |
		  ("c" .$ (COMMENT))).%

// Token
abstract class Token extends Serializable 
case class T_KEYWORD(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_STRING(s: String) extends Token
case class T_PAREN(s: String) extends Token
case object T_SEMI extends Token
case class T_ID(s: String) extends Token
case class T_NUM(n: Int) extends Token

val token : PartialFunction[(String, String), Token] = {
  case ("k", s) => T_KEYWORD(s)
  case ("o", s) => T_OP(s)
  case ("str", s) => T_STRING(s)
  case ("p", s) => T_PAREN(s)
  case ("s", _) => T_SEMI
  case ("i", s) => T_ID(s)
  case ("n", s) => T_NUM(s.toInt)
}

// Tokenise
def tokenise(s: String) : List[Token] = 
  lexing_simp(WHILE_REGS, s).collect(token)


// Q2 Tests

//lex_simp(NTIMES("a", 3), "aaa".toList)
//lex_simp(NTIMES(("a" | ONE), 3), "aa".toList)

// Q3 Programs

//println("Fib")
//println(tokenise(os.read(os.pwd / "examples" / "fib.while")))

//println("Factors")
//println(tokenise(os.read(os.pwd / "examples" / "factors.while")))

//println("Loops")
//println(tokenise(os.read(os.pwd / "examples" / "loops.while")))

//println("Collatz")
//println(tokenise(os.read(os.pwd / "examples" / "collatz.while")))

//println("Collatz 2")
//println(tokenise(os.read(os.pwd / "examples" / "collatz2.while")))

//println("Primes")
//println(tokenise(os.read(os.pwd / "examples" / "primes.while")))


@main
def test(file: String) = {
  val contents = os.read(os.pwd / "examples" / file)
  println(s"Lex $file: ")
  println(tokenise(contents))
}
