// A template of the simple matcher with simplification 
// of derivatives to be used in CW1 (OPTIONAL)


abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 

case class RANGE(cs: Set[Char]) extends Rexp             // set of characters
case class PLUS(r: Rexp) extends Rexp                    // plus
case class OPTIONAL(r: Rexp) extends Rexp                // optional
case class INTER(r1: Rexp, r2: Rexp) extends Rexp        // intersection
case class NTIMES(r: Rexp, n: Int) extends Rexp          // n-times
case class UPTO(r: Rexp, n: Int) extends Rexp            // up n-times
case class FROM(r: Rexp, n: Int) extends Rexp            // from n-times
case class BETWEEN(r: Rexp, n: Int, m: Int) extends Rexp // between nm-times
case class NOT(r: Rexp) extends Rexp                     // not
case class CFUN(f: Char => Boolean) extends Rexp  
 


// the nullable function: tests whether the regular 
// expression can recognise the empty string
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case PLUS (r)=> nullable(r)
  case CHAR(_) => false
  case OPTIONAL(_) => true
  case STAR(_) => true
  case NOT(r1) => !nullable(r1)
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  case UPTO(r1, i) => if (i == 0) true else nullable(r1)
  case INTER(r1, r2) => nullable(r1) && nullable(r2)
  case FROM(r1, i)  => if (i == 0) true else nullable(r1)
  case BETWEEN(r1, i, j) => if (i == 0) true else nullable(r1)
  case RANGE(_) => false
  case CFUN(f) => false

}

// the derivative of a regular expression w.r.t. a character
def der(c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case NTIMES(r, i) => 
    if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
  case OPTIONAL(r1) => der(c,r1)
  case PLUS(r1) => SEQ(der(c, r1), ALT(PLUS(r1), ONE))
  case UPTO(r1, n) => if (n == 0) ZERO else SEQ(der(c,r1), UPTO(r1,n-1))
  case BETWEEN(r1, n, m) => if (m == 0) ZERO else SEQ(der(c, r1), BETWEEN(r1, math.max(0, n - 1), m - 1))
  case NOT(r1) => NOT(der(c,r1))
  case INTER(r1, r2) => INTER(der(c,r1), der(c,r2))
  case FROM(r1, n) => if (n <= 1) SEQ(der(c,r1), STAR(r1)) else SEQ(der(c,r1), FROM(r1,n-1))
  case RANGE(cs) => if (cs.contains(c)) ONE else ZERO
  case CFUN(f) => if (f(c)) ONE else ZERO





}

// simplification
def simp(r: Rexp) : Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT (r1s, r2s)
  }
  case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
}

def cfun_char(c: Char): Rexp =
  CFUN(x => x == c)

def cfun_range(cs: Set[Char]): Rexp =
  CFUN(x => cs.contains(x))

def ALL: Rexp =
  CFUN(_ => true)


// the derivative w.r.t. a string (iterates der and simp)
def ders(s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, simp(der(c, r)))

}

// the main matcher function
def matcher(r: Rexp, s: String) : Boolean = 
  nullable(ders(s.toList, r))
//test

// lowercase, digits, underscore, or dot
val nameChar = CFUN(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_' || c == '.')

val localPart = PLUS(nameChar)
val domain = PLUS(CFUN(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '.'))
val tld = BETWEEN(CFUN(c => c >= 'a' && c <= 'z'), 2, 6)
val emailRexp =
  SEQ(localPart,SEQ(CHAR('@'),SEQ(domain,SEQ(CHAR('.'), tld))))


val comment = SEQ(
  CHAR('/'),
  SEQ(
    CHAR('*'),
    SEQ(
      NOT(
        SEQ(
          STAR(ALL),
          SEQ(
            CHAR('*'),
            SEQ(CHAR('/'), STAR(ALL))
          )
        )
      ),
      SEQ(CHAR('*'), CHAR('/'))
    )
  )
)