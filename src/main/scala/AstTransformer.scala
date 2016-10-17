import scala.xml._
import scala.xml.transform._

import scala.util.{Try, Success, Failure}
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input.{NoPosition, Position, Reader}

sealed trait Token
case class NUMBER(value: Double) extends Token
case class IDENTIFIER(name: String) extends Token
case object PLUS extends Token
case object MINUS extends Token
case object STAR extends Token
case object SLASH extends Token
case object CARET extends Token
case object OPENPAREN extends Token
case object CLOSEPAREN extends Token

sealed trait MathMLExpr
final case class Const(a: Double) extends MathMLExpr
final case class Var(name: String) extends MathMLExpr
final case class Add(a: MathMLExpr, b: MathMLExpr) extends MathMLExpr
final case class Sub(a: MathMLExpr, b: MathMLExpr) extends MathMLExpr
final case class Mul(a: MathMLExpr, b: MathMLExpr) extends MathMLExpr
final case class Div(a: MathMLExpr, b: MathMLExpr) extends MathMLExpr
final case class Pow(a: MathMLExpr, b: MathMLExpr) extends MathMLExpr

import cats.free.Free
//type MathMLExpr[T] = Free[MathMLExprT, T]
//def liftExpr[T](expr: MathMLExprT[T]): MathMLExpr[Double] =
//  liftF[MathMLExprA, T](expr)

trait Env {
  def get(name: String): Double
}

case class MapEnv(env: Map[String, Double]) extends Env {
  def get(name: String): Double = env.lift(name).get
}

object AstTransformer {

  def transform(elem: Elem): MathMLExpr = {
    require(elem.label == "math")

    println(s"before: $elem")
    
    val flattened =
      (ExpandTransformer
        andThen SimplifyTransformer)(elem)
      .child
      .map { _.asInstanceOf[Elem] }

    val tokens = lex(flattened)

    println(s"tokens: $tokens")

    val ast = MathMLExprParser(tokens)

    
    println(s"after: $ast")

    ast.get
  }

  def identifiers(expr: MathMLExpr): Set[String] = expr match {
    case Add(a, b) => identifiers(a) ++ identifiers(b)
    case Sub(a, b) => identifiers(a) ++ identifiers(b)
    case Mul(a, b) => identifiers(a) ++ identifiers(b)
    case Div(a, b) => identifiers(a) ++ identifiers(b)
    case Pow(a, b) => identifiers(a) ++ identifiers(b)
    case Var(a) => Set(a)
    case _ => Set.empty
  }


  //def toFree[F](expr: MathMLExpr): Free[F, Double]
  def eval(expr: MathMLExpr, env: Env): Double = ???

  private def lex(input: Seq[Elem]): Seq[Token] =
    input.map {
      case <mn>{number: Text}</mn> => NUMBER(number.data.toDouble)
      case <mi>{name: Text}</mi> => IDENTIFIER(name.data)
      case <mo>+</mo> => PLUS
      case <mo>-</mo> => MINUS
      case <mo>*</mo> => STAR
      case <mo>/</mo> => SLASH
      case <mo>^</mo> => CARET
      case <mo>(</mo> => OPENPAREN
      case <mo>)</mo> => CLOSEPAREN
    }
}

object MRowToParens extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case <mrow>{children @ _*}</mrow> =>
      //println(s"mrow child $children")
        (<mo>(</mo> +:
        children) :+
        <mo>)</mo>
    case other => other
  }
}
object FracInfix extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case <mfrac>{children @ _*}</mfrac> =>
      Seq(
        <mo>(</mo>,
          children(0),
        <mo>)</mo>,
        <mo>/</mo>,
        <mo>(</mo>,
          children(1),
        <mo>)</mo>)
    case other => other
  }
}
object PowInfix extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case <msup>{children @ _*}</msup> =>
      Seq(
        <mo>(</mo>,
          children(0),
        <mo>)</mo>,
        <mo>^</mo>,
        <mo>(</mo>,
          children(1),
        <mo>)</mo>)
    case other => other
  }
}
object ExplicitOps extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case <mo>{"\u00b7"}</mo> |
         <mo>{"\u2062"}</mo> |
         <mo>{" "}</mo> =>
      <mo>*</mo>
    case other => other
  }
}
object SimplifyParens extends RewriteRule {
  val parens = Set("(", ")")
  override def transform(n: Node): Seq[Node] = n match {
    case <math>{children @ _*}</math> =>
      val simplified = children.foldLeft[Seq[Elem]](Seq()) {
          case (Seq(), elem: Elem) => Seq(elem)
          case (result@_ :+ last, elem: Elem) =>
            if (last.text == elem.text && parens.contains(elem.text)) {
              result
            } else {
              result :+ elem
            }
        }
      <math>{simplified}</math>
    case other => other
  }
}
object ExpandTransformer extends RuleTransformer(
  FracInfix,
  PowInfix)
object SimplifyTransformer extends RuleTransformer(
  MRowToParens,
  SimplifyParens,
  ExplicitOps)

object MathMLExprParser extends Parsers {
  override type Elem = Token

  class MathMLExprReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[Token] = new MathMLExprReader(tokens.tail)
  }

  def apply(tokens: Seq[Token]): Try[MathMLExpr] = {
    val reader = new MathMLExprReader(tokens)
    expr(reader) match {
      case NoSuccess(msg, next) => scala.util.Failure(new java.lang.Error(s"${next.pos.line} ${next.pos.column}"))
      case Success(result, _) => scala.util.Success(result)
    }
  }
  // Grammer #10 from http://math.purduecal.edu/~rlkraft/cs31600-2012/chapter03/syntax-examples.html 
  lazy val expr: Parser[MathMLExpr] =
    (term ~ (PLUS | MINUS) ~ term) ^^ {
      case t1 ~ PLUS ~ t2 => Add(t1, t2)
      case t1 ~ MINUS ~ t2 => Sub(t1, t2)
    } | term

  lazy val term: Parser[MathMLExpr] =
    (factor ~ (STAR | SLASH) ~ factor) ^^ {
      case t ~ STAR ~ f => Mul(t, f)
      case t ~ SLASH ~ f => Div(t, f)
    } | factor

  lazy val factor: Parser[MathMLExpr] =
    (base ~ CARET ~ exponent) ^^ {
      case b ~ _ ~ e => Pow(b, e)
    } | base

  lazy val base: Parser[MathMLExpr] =
    (OPENPAREN ~> expr <~ CLOSEPAREN) | number | identifier

  lazy val exponent: Parser[MathMLExpr] = base

  lazy val number: Parser[Const] =
    accept("number", { case NUMBER(n) => Const(n) })

  lazy val identifier: Parser[MathMLExpr] =
    accept("identifier", { case IDENTIFIER(s) => Var(s) })
}
