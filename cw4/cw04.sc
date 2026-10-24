// Compiler for the JVM
//====================== 

// CW3
//=====



//
// The main idea is to extend the parser form the lectures 
// (which processes strings) to a parser that processes
// tokens. For this you need to use the lexer from CW2 and
// possibly adjust the lexing regular expressions accordingly.


// IMPORTANT:
//
// You should include the lexer from CW2, which defines
// Tokens and tokenise. Copy the adapted files to this
// directory and add them to the repository.
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


val KEYWORD : Rexp = "skip" | "while" | "do" | "if" | "then" | "else" | "read" | "write" | "true" | "false" |"for"| "upto" | "break"
val OP: Rexp = "+=" | "-=" | "*=" | ":=" | "==" | "!=" | ">=" | "<=" | "&&" | "||" | "=" | "-" | "+" | "*" | "/" | "%" | "<" | ">"
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
// parser combinators

type IsSeq[I] = I => Seq[?]

abstract class Parser[I, T](using is: IsSeq[I])  {
  def parse(in: I): Set[(T, I)]  

  def parse_all(in: I) : Set[T] =
    for ((hd, tl) <- parse(in); 
        if is(tl).isEmpty) yield hd
}

// parser combinators

// alternative parser
class AltParser[I : IsSeq, T](p: => Parser[I, T], 
                              q: => Parser[I, T]) extends Parser[I, T] {
  def parse(in: I) = p.parse(in) ++ q.parse(in)   
}

// sequence parser
class SeqParser[I: IsSeq, T, S](p: => Parser[I, T], 
                                q: => Parser[I, S]) extends Parser[I, (T, S)] {
  def parse(in: I) = 
    for ((hd1, tl1) <- p.parse(in); 
         (hd2, tl2) <- q.parse(tl1)) yield ((hd1, hd2), tl2)
}

// map parser
class MapParser[I : IsSeq, T, S](p: => Parser[I, T], 
                         f: T => S) extends Parser[I, S] {
  def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
}


// some convenient syntax for parser combinators
extension [I: IsSeq, T](p: Parser[I, T]) {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
}

// Abstract Syntax Trees
abstract class Stmt
abstract class AExp
abstract class BExp

type Block = List[Stmt]

case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class For(id: String, from: AExp, to: AExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Read(s: String) extends Stmt
case class WriteVar(s: String) extends Stmt     // for printing values of variables
case class WriteString(s: String) extends Stmt  // for printing strings
case object Break extends Stmt

case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
case class Lop(o: String, b1: BExp, b2: BExp) extends BExp

given IsSeq[List[Token]] = (l: List[Token]) => l
                 
// Atomic token parsers
case class OpParser(op: String) extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_OP(s) :: rest if s == op => Set((s, rest))
    case _ => Set()
  }
}

case object IdParser extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_ID(s) :: rest => Set((s, rest))
    case _ => Set()
  }
}

case object NumParser extends Parser[List[Token], Int] {
  def parse(ts: List[Token]) = ts match {
    case T_NUM(n) :: rest => Set((n, rest))
    case _ => Set()
  }
}

case class ParenParser(p: String) extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_PAREN(s) :: rest if s == p => Set((s, rest))
    case _ => Set()
  }
}

case object StringParser extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_STRING(s) :: rest => Set((s, rest))
    case _ => Set()
  }
}
case object SemiParser extends Parser[List[Token], Unit] {
  def parse(ts: List[Token]) = ts match {
    case T_SEMI :: rest => Set(((), rest))
    case _ => Set()
  }
}
case class KeywordParser(kw: String) extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_KEYWORD(s) :: rest if s == kw => Set((s, rest))
    case _ => Set()
  }
}

// Parser rules
lazy val AExp: Parser[List[Token], AExp] = 
  (Te ~ OpParser("+") ~ AExp).map[AExp]{ case ((x, _), z) => Aop("+", x, z) } ||
  (Te ~ OpParser("-") ~ AExp).map[AExp]{ case ((x, _), z) => Aop("-", x, z) } || 
  Te

lazy val Te: Parser[List[Token], AExp] = 
  (Fa ~ OpParser("*") ~ Te).map[AExp]{ case ((x, _), z) => Aop("*", x, z) } || 
  (Fa ~ OpParser("/") ~ Te).map[AExp]{ case ((x, _), z) => Aop("/", x, z) } ||
  (Fa ~ OpParser("%") ~ Te).map[AExp]{ case ((x, _), z) => Aop("%", x, z) } ||
  Fa

lazy val Fa: Parser[List[Token], AExp] = 
  (ParenParser("(") ~ AExp ~ ParenParser(")")).map{ case ((_, y), _) => y } || 
  IdParser.map(Var(_)) || 
  NumParser.map(Num(_))


lazy val BExp: Parser[List[Token], BExp] = 
  (AExp ~ OpParser("==") ~ AExp).map[BExp]{ case ((x, _), z) => Bop("==", x, z) } || 
  (AExp ~ OpParser("!=") ~ AExp).map[BExp]{ case ((x, _), z) => Bop("!=", x, z) } || 
  (AExp ~ OpParser("<=") ~ AExp).map[BExp]{ case ((x, _), z) => Bop("<=", x, z) } || 
  (AExp ~ OpParser(">=") ~ AExp).map[BExp]{ case ((x, _), z) => Bop(">=", x, z) } || 
  (AExp ~ OpParser("<") ~ AExp).map[BExp]{ case ((x, _), z) => Bop("<", x, z) } || 
  (AExp ~ OpParser(">") ~ AExp).map[BExp]{ case ((x, _), z) => Bop(">", x, z) } ||
  (ParenParser("(") ~ BExp ~ ParenParser(")") ~ OpParser("&&") ~ BExp).map[BExp]{ case ((((_, y), _), _), v) => Lop("&&", y, v) } ||
  (ParenParser("(") ~ BExp ~ ParenParser(")") ~ OpParser("||") ~ BExp).map[BExp]{ case ((((_, y), _), _), v) => Lop("||", y, v) } ||
  (KeywordParser("true").map[BExp]{ _ => True }) || 
  (KeywordParser("false").map[BExp]{ _ => False }) ||
  (ParenParser("(") ~ BExp ~ ParenParser(")")).map[BExp]{ case ((_, x), _) => x }

lazy val Stmt: Parser[List[Token], Stmt] = 
  (KeywordParser("skip").map[Stmt]{_ => Skip }) ||
  (IdParser ~ OpParser("+=") ~ AExp).map[Stmt]{ case ((x, _), z) => Assign(x, Aop("+", Var(x), z)) } ||
  (IdParser ~ OpParser("-=") ~ AExp).map[Stmt]{ case ((x, _), z) => Assign(x, Aop("-", Var(x), z)) } ||
  (IdParser ~ OpParser("*=") ~ AExp).map[Stmt]{ case ((x, _), z) => Assign(x, Aop("*", Var(x), z)) } ||
  (IdParser ~ OpParser(":=") ~ AExp).map[Stmt]{ case ((x, _), z) => Assign(x, z) } ||
  (KeywordParser("write") ~ ParenParser("(") ~ IdParser ~ ParenParser(")")).map[Stmt]{ 
    case (((_, _), id), _) => WriteVar(id) 
  } ||
  (KeywordParser("write") ~ ParenParser("(") ~ StringParser ~ ParenParser(")")).map[Stmt]{ 
    case (((_, _), str), _) => WriteString(str)
  } ||
  (KeywordParser("write") ~ IdParser).map[Stmt]{ case (_, id) => WriteVar(id) } ||
  (KeywordParser("write") ~ StringParser).map[Stmt]{ case (_, str) => WriteString(str) } ||
  (KeywordParser("read") ~ ParenParser("(") ~ IdParser ~ ParenParser(")")).map[Stmt]{ 
    case (((_, _), y), _) => Read(y) 
  } ||
  (KeywordParser("read") ~ IdParser).map[Stmt]{ case (_, y) => Read(y) } ||
  (KeywordParser("if") ~ BExp ~ KeywordParser("then") ~ Block ~ KeywordParser("else") ~ Block)
    .map[Stmt]{ case (((((_, y), _), u), _), w) => If(y, u, w) } ||
  (KeywordParser("while") ~ BExp ~ KeywordParser("do") ~ Block).map[Stmt]{ 
    case (((_, y), _), w) => While(y, w) 
  } ||
  (KeywordParser("for") ~ IdParser ~ OpParser(":=") ~ AExp ~
   KeywordParser("upto") ~ AExp ~ KeywordParser("do") ~ Block)
  .map {
    case (((((((_, id), _), from), _), to), _), bl) => For(id, from, to, bl)
  } ||
  (KeywordParser("break").map[Stmt]{ _ => Break })




lazy val Stmts: Parser[List[Token], Block] = 
  (Stmt ~  SemiParser ~ Stmts).map[Block]{ case ((x, _), z) => x :: z } ||
  (Stmt.map[Block]{ s => List(s) })

lazy val Block: Parser[List[Token], Block] = 
  ((ParenParser("{") ~ Stmts ~ ParenParser("}")).map{ case ((_, y), _) => y } || 
  (Stmt.map(s => List(s))))


// Interpreter
//=============

// Import needed to take an int as input from the user
import scala.io.StdIn.readInt

type Env = Map[String, Int]

def eval_aexp(a: AExp, env: Env) : Int = 
 a match {
    case Num(i) => i
    case Var(s) => env(s)
    case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
    case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
    case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
    case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
    case Aop("%", a1, a2) => eval_aexp(a1, env) % eval_aexp(a2, env)
  }
def eval_bexp(b: BExp, env: Env) : Boolean = 
  b match {
    case True => true
    case False => false
    case Bop("==", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
    case Bop("!=", a1, a2) => eval_aexp(a1, env) != eval_aexp(a2, env)
    case Bop("<=", a1, a2) => eval_aexp(a1, env) <= eval_aexp(a2, env)
    case Bop(">=", a1, a2) => eval_aexp(a1, env) >= eval_aexp(a2, env)
    case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
    case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
    case Lop("&&", b1, b2) => eval_bexp(b1, env) && eval_bexp(b2, env)
    case Lop("||", b1, b2) => eval_bexp(b1, env) || eval_bexp(b2, env)
  }

def eval_stmt(s: Stmt, env: Env) : Env = 
  s match {
    case Skip => env
    case Assign(x, a) => env + (x -> eval_aexp(a, env))
    case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env) 
    case While(b, bl) => 
      if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env))
      else env
    case Read(x) => 
      println(s"Enter value for $x: ")
      env + (x -> readInt())
    case WriteVar(x) => 
      print(env.getOrElse(x, 0))
      env
    case WriteString(s) => 
      print(s)
      env
    case For(id, from, to, bl) =>
      val start = eval_aexp(from, env)
      val end = eval_aexp(to, env)  
      def loop(i: Int, envLoop: Env): Env = {
        if (i > end) envLoop
        else {
          val newEnv = eval_bl(bl, envLoop + (id -> i))
          loop(i + 1, newEnv)
        }
      }
      loop(start, env)
  }

def eval_bl(bl: Block, env: Env) : Env = bl match {
    case Nil => env
    case s::bl => eval_bl(bl, eval_stmt(s, env))
  }
// main eval function for the interpreter
def eval(bl: Block) : Env = eval_bl(bl, Map())

// fresh variables
val beginning = """
.class public XXX.XXX
.super java/lang/Object

.method public static write(I)V
    .limit locals 1
    .limit stack 2
    getstatic java/lang/System/out Ljava/io/PrintStream;
    iload 0
    invokevirtual java/io/PrintStream/print(I)V
    return
.end method

.method public static read()I
    .limit locals 1
    .limit stack 3

    new java/util/Scanner          ; create new Scanner
    dup
    getstatic java/lang/System/in Ljava/io/InputStream;
    invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V

    invokevirtual java/util/Scanner/nextInt()I   ; read int
    ireturn
.end method


.method public static main([Ljava/lang/String;)V
    .limit locals 200
    .limit stack 200

  ;COMPILED CODE STARTS 

"""

val ending = """
    ;COMPILED CODE ENDS

    return
    .end method"""
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// for JVM instructions and labels
extension (sc: StringContext) {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
}


// you can make changes to the arguments of the auxiliary
// compile_* functions, but the main compile needs to take 
// a block and a class_name as argument, and produce the 
// j-file as string.
var loopStack = List[String]()       // keeps track of enclosing loop end labels
val globalEndLabel = Fresh("ProgramEnd")  // for breaks outside loops

def compile_op(op: String) : String = op match {
  case "+"  => i"iadd"
  case "-" => i"isub"
  case "*" => i"imul"
  case "/" => i"idiv"
  case "%" => i"irem"
}

def compile_aexp(a: AExp, env : Env) : String = a match {
  case Num(i) => i"ldc $i"
  case Var(s) => i"iload ${env(s)} \t\t; $s"
  case Aop(op, a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ compile_op(op)
}

def compile_bexp(b: BExp, env : Env, jmp: String) : String = b match{
  case True => ""
  case False => i"goto $jmp"
  case Bop("==", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpne $jmp"
  case Bop("!=", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpeq $jmp"
  case Bop("<=", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpgt $jmp"   
  case Bop(">=", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmplt $jmp"
  case Bop("<", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpge $jmp"  
  case Bop(">", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmple $jmp" 
  case Lop("&&", b1, b2) =>
  compile_bexp(b1, env, jmp) ++
  compile_bexp(b2, env, jmp)

  case Lop("||", b1, b2) =>
  val next = Fresh("or_next")
  compile_bexp(b1, env, next) ++  // if b1 false → jump to next
  l"goto $next" ++                // if b1 true → skip b2 (success)
  l"$next" ++
  compile_bexp(b2, env, jmp)      // evaluate b2

}

def compile_stmt(s: Stmt, env: Env) : (String, Env) = s match {
  case Skip => ("", env)
  case Assign(x, a) => {
    val index = env.getOrElse(x, env.keys.size)
    (compile_aexp(a, env) ++ i"istore $index \t\t; $x", env + (x -> index))
  }
  case If(b, bl1, bl2) => {
    val if_else = Fresh("If_else")
    val if_end = Fresh("If-end")
    val (instrs1, env1) = compile_block(bl1, env)
    val (instrs2, env2) = compile_block(bl2, env1)
    (compile_bexp(b, env, if_else) ++
     instrs1 ++
     i"goto $if_end" ++
     l"$if_else" ++
     instrs2 ++
     l"$if_end", env2)
  }
  case While(b, bl) => {
    val loop_begin = Fresh("Loop_begin")
    val loop_end   = Fresh("Loop_end")

    loopStack = loop_end :: loopStack          // push loop end
    val (instrs1, env1) = compile_block(bl, env)
    loopStack = loopStack.tail                 // pop after compiling loop body

    (l"$loop_begin" ++
    compile_bexp(b, env, loop_end) ++
    instrs1 ++
    i"goto $loop_begin" ++
    l"$loop_end",
    env1)
  }

  case For(id, from, to, bl) => {
  val loop_begin = Fresh("For_Loop_begin") 
  val loop_end   = Fresh("For_Loop_end") 

  val index = env.getOrElse(id, env.size) 
  val envWithId = env + (id -> index) 

  loopStack = loop_end :: loopStack          // push loop end
  val (instrs1, env1) = compile_block(bl, envWithId) 
  loopStack = loopStack.tail                 // pop after loop

  (
    compile_aexp(from, envWithId) ++ 
    i"istore $index" ++ 
    l"$loop_begin" ++ 
    i"iload $index" ++ 
    compile_aexp(to, envWithId) ++ 
    i"if_icmpgt $loop_end" ++ 
    instrs1 ++ 
    i"iload $index" ++ 
    i"ldc 1" ++ 
    i"iadd" ++ 
    i"istore $index" ++ 
    i"goto $loop_begin" ++ 
    l"$loop_end",
    env1 
  )
  }
  case Break =>
  val target = loopStack.headOption.getOrElse(globalEndLabel)
  (i"goto $target", env)

  
  case WriteVar(x) =>
  (
    i"iload ${env(x)}" ++
    i"invokestatic XXX/XXX/write(I)V",
    env
  )

  case WriteString(str) =>
  (
    i"getstatic java/lang/System/out Ljava/io/PrintStream;" ++
    i"ldc $str" ++
    i"invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V",
    env
  )
  case Read(x) =>
  val index = env.getOrElse(x, env.size)
  (
    i"invokestatic XXX/XXX/read()I" ++
    i"istore $index \t\t; $x",
    env + (x -> index)
  )


}

def compile_block(bl: Block, env: Env) : (String, Env) = bl match {
  case Nil => ("", env)
  case s::bl => {
    val (instrs1, env1) = compile_stmt(s, env)
    val (instrs2, env2) = compile_block(bl, env1)
    (instrs1 ++ instrs2, env2)
  }
}


def compile(bl: Block, class_name: String) : String = {
  val instructions = compile_block(bl, Map.empty)._1
  (beginning ++ instructions ++ l"$globalEndLabel" ++ ending).replaceAllLiterally("XXX", class_name)
}

val fib_test = 
  List(
    Assign("n", Num(9)),
    Assign("minus1", Num(0)),
    Assign("minus2", Num(1)),
    Assign("temp", Num(0)),
    While(Bop("<", Num(0), Var("n")),
      List(Assign("temp", Var("minus2")),
        Assign("minus2", Aop("+", Var("minus1"), Var("minus2"))),
        Assign("minus1", Var("temp")),
        Assign("n", Aop("-", Var("n"), Num(1)))
      )
    ),
    WriteVar("minus1")
  )


def test_string(prog: String, cname: String) = {
  val tks = tokenise(prog) // has to adapted from earlier CWs
  println(tks)
  val ast = Stmts.parse_all(tks).head // has to adapted from earlier CWs
  println(ast)
  val code = compile(ast, cname)
  println(code)
  os.write.over(os.pwd / (cname ++ ".j"), code)
}

