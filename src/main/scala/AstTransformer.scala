package mathml

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

sealed trait MathMLExprOp[A]
final case class Const(a: Double) extends MathMLExprOp[Double]
final case class Var(name: String) extends MathMLExprOp[Double]
final case class Add(a: Double, b: Double) extends MathMLExprOp[Double]
final case class Sub(a: Double, b: Double) extends MathMLExprOp[Double]
final case class Mul(a: Double, b: Double) extends MathMLExprOp[Double]
final case class Div(a: Double, b: Double) extends MathMLExprOp[Double]
final case class Pow(a: Double, b: Double) extends MathMLExprOp[Double]

// Free applicative DSL
import cats._
import cats.free.FreeApplicative
import cats.free.FreeApplicative.lift
import cats.free.Free
import cats.free.Free.liftF

import cats.implicits._

trait Env {
  def get(name: String): Double
}

case class MapEnv(env: Map[String, Double]) extends Env {
  def get(name: String): Double = env.lift(name).get
}

package object MathMLDSL {
  type MathMLExpr[A] = Free[MathMLExprOp, A]
  
  def const(a: Double): MathMLExpr[Double] = liftF(Const(a))
  def variable(name: String): MathMLExpr[Double] = liftF(Var(name))
  def add(a: Double, b: Double): MathMLExpr[Double] = liftF(Add(a, b))
  def sub(a: Double, b: Double): MathMLExpr[Double] = liftF(Sub(a, b))
  def mul(a: Double, b: Double): MathMLExpr[Double] = liftF(Mul(a, b))
  def div(a: Double, b: Double): MathMLExpr[Double] = liftF(Div(a, b))
  def pow(a: Double, b: Double): MathMLExpr[Double] = liftF(Pow(a, b))
}

object AstTransformer {
  import MathMLDSL._

  // Free Applicative defs
  // External API
  def parseMathML(elem: Elem): MathMLExpr[Double] = {
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

  //def identifiers[T](expr: MathMLExpr[T]): Set[String] = expr match {
  //  case Add(a, b) => identifiers(a) ++ identifiers(b)
  //  case Sub(a, b) => identifiers(a) ++ identifiers(b)
  //  case Mul(a, b) => identifiers(a) ++ identifiers(b)
  //  case Div(a, b) => identifiers(a) ++ identifiers(b)
  //  case Pow(a, b) => identifiers(a) ++ identifiers(b)
  //  case Var(a) => Set(a)
  //  case _ => Set.empty
  //}


  def eval[T](expr: MathMLExpr[T], env: Env): T = ???

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
  import MathMLDSL._
  override type Elem = Token

  val C = implicitly[cats.Cartesian[MathMLExpr]]

  class MathMLExprReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[Token] = new MathMLExprReader(tokens.tail)
  }

  def apply(tokens: Seq[Token]): Try[MathMLExpr[Double]] = {
    val reader = new MathMLExprReader(tokens)
    expr(reader) match {
      case NoSuccess(msg, next) => scala.util.Failure(new java.lang.Error(s"${next.pos.line} ${next.pos.column}"))
      case Success(result, _) => scala.util.Success(result)
    }
  }
  // Grammer #10 from http://math.purduecal.edu/~rlkraft/cs31600-2012/chapter03/syntax-examples.html 
  lazy val expr: Parser[MathMLExpr[Double]] =
    (term ~ (PLUS | MINUS) ~ term) ^^ {
      case t1 ~ PLUS ~ t2 =>
        C.product(t1, t2).flatMap {
          case (t1, t2) => add(t1, t2)
        }
      case t1 ~ MINUS ~ t2 =>
        C.product(t1, t2).flatMap {
          case (t1, t2) => sub(t1, t2)
        }
    } | term

  lazy val term: Parser[MathMLExpr[Double]] =
    (factor ~ (STAR | SLASH) ~ factor) ^^ {
      case t ~ STAR ~ f =>
        C.product(t, f).flatMap {
        case (t, f) => mul(t, f)
      }
      case t ~ SLASH ~ f =>
        C.product(t, f).flatMap {
          case (t, f) => div(t, f)
        }
    } | factor

  lazy val factor: Parser[MathMLExpr[Double]] =
    (base ~ CARET ~ exponent) ^^ {
      case b ~ _ ~ e =>
        C.product(b, e).flatMap {
          case (b, e) => pow(b, e)
        }
    } | base

  lazy val base: Parser[MathMLExpr[Double]] =
    (OPENPAREN ~> expr <~ CLOSEPAREN) | number | identifier

  lazy val exponent: Parser[MathMLExpr[Double]] = base

  lazy val number: Parser[MathMLExpr[Double]] =
    accept("number", { case NUMBER(n) => const(n) })

  lazy val identifier: Parser[MathMLExpr[Double]] =
    accept("identifier", { case IDENTIFIER(s) => variable(s) })
}

object Compiler {
  import cats.~>
  def impure[F[_]](env: Map[String, Double])(implicit F: Monad[F]): MathMLExprOp ~> F =
    new (MathMLExprOp ~> F) {
      def apply[A](fa: MathMLExprOp[A]): F[A] =
        fa match {
          case Const(a) => F.pure(a)
          case Var(a) => F.pure(env.lift(a).get)
          case Add(a, b) => F.pure(a + b)
          case Sub(a, b) => F.pure(a - b)
          case Mul(a, b) => F.pure(a * b)
          case Div(a, b) => F.pure(a / b)
          case Pow(a, b) => F.pure(Math.pow(a, b))
        }
    }
}

