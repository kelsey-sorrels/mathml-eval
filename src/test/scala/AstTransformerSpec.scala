package mathml

import collection.mutable.Stack
import org.scalatest._

import scala.util.{Failure, Success, Try}
import scala.xml._

import cats._
import cats.implicits._

class ExampleSpec extends WordSpec with Matchers with TryValues {

  val ndviMathML = """<math xmlns="http://www.w3.org/1998/Math/MathML"><mrow><mfrac><mrow><mrow><mrow><mi mathcolor="#443399">MIR</mi><mo>-</mo><mi mathcolor="#443399">NIR</mi></mrow></mrow></mrow><mrow><mrow><mrow><mi mathcolor="#443399">MIR</mi><mo>+</mo><mi mathcolor="#443399">NIR</mi></mrow></mrow></mrow></mfrac></mrow></math>"""

  "A MathML expression" should {
    "be parseable" in {
      val xml = XML.loadString(ndviMathML)
      val result = AstTransformer.parseMathML(xml)
      result should be a 'success
    }
    "find identifiers" in {
      val xml = XML.loadString(ndviMathML)
      val result = AstTransformer.parseMathML(xml)
      result should be a 'success
      result.get._1 shouldBe Set("NIR", "MIR")
    }
    "be evaluable" in {
      val xml = XML.loadString(ndviMathML)
      val result = AstTransformer.parseMathML(xml)
      result should be a 'success
      val env = Map("NIR" -> 0.5,
                    "MIR" -> 0.5)
      AstTransformer.eval(result.get._2, env) shouldBe Success(0.0)

    }
  }
  
  "A MathMLExprEvaluator" should {
    "evaluate constants" in {
      val result = MathMLExprParser(Seq(NUMBER(1)))
        .flatMap { program =>
          program.foldMap(Compiler.impure[Try](Map()))
        }
      result should be(Success(1))
    }
    "evaluate variables" in {
      val result = MathMLExprParser(Seq(IDENTIFIER("a")))
        .flatMap { program =>
          program.foldMap(Compiler.impure[Try](Map("a" -> 2)))
        }
      result should be(Success(2))
    }
  }

  //"A MathMLExprParser" should {
    //"parse numbers" in {
    //  MathMLExprParser(Seq(NUMBER(1))) should be(Success(Const(1)))
    //}
    //"parse variables" in {
    //  MathMLExprParser(Seq(IDENTIFIER("a"))) should be(Success(Var("a")))
    //}
    //"parse addition" in {
    //  MathMLExprParser(Seq(NUMBER(1),
    //                       PLUS,
    //                       NUMBER(1))) should be(Success(Add(Const(1), Const(1))))
    //}
    //"parse subtraction" in {
    //  MathMLExprParser(Seq(NUMBER(1),
    //                       MINUS,
    //                       NUMBER(1))) should be(Success(Sub(Const(1), Const(1))))
    //}
    //"parse multiplication" in {
    //  MathMLExprParser(Seq(NUMBER(1),
    //                       STAR,
    //                       NUMBER(1))) should be(Success(Mul(Const(1), Const(1))))
    //}
    //"parse division" in {
    //  MathMLExprParser(Seq(NUMBER(1),
    //                       SLASH,
    //                       NUMBER(1))) should be(Success(Div(Const(1), Const(1))))
    //}
    //"parse operators with precedence" in {
    //  // 1 + 2 * 3 ^ 4
    //  val parsed = MathMLExprParser(Seq(NUMBER(1),
    //                       PLUS,
    //                       NUMBER(2),
    //                       STAR,
    //                       NUMBER(3),
    //                       CARET,
    //                       NUMBER(4)))
    //  parsed should be(Success(
    //    Add(
    //      Const(1),
    //      Mul(
    //        Const(2),
    //        Pow(
    //          Const(3),
    //          Const(4))))))
    //}
    //"parse reversed operators with precedence" in {
    //  // 1 ^ 2 * 3 + 4
    //  val parsed = MathMLExprParser(Seq(NUMBER(1),
    //                       CARET,
    //                       NUMBER(2),
    //                       STAR,
    //                       NUMBER(3),
    //                       PLUS,
    //                       NUMBER(4)))
    //  parsed should be(Success(
    //    Add(
    //      Mul(
    //        Pow(
    //          Const(1),
    //          Const(2)),
    //        Const(3)),
    //      Const(4))))
    //}
    //"parse parens for operators with precedence" in {
    //  // ((1 + 2) * 3) ^ 4
    //  val parsed = MathMLExprParser(Seq(
    //    OPENPAREN,
    //    OPENPAREN,
    //    NUMBER(1),
    //    PLUS,
    //    NUMBER(2),
    //    CLOSEPAREN,
    //    STAR,
    //    NUMBER(3),
    //    CLOSEPAREN,
    //    CARET,
    //    NUMBER(4)))
    //  parsed should be(Success(
    //    Pow(
    //      Mul(
    //        Add(
    //          Const(1),
    //          Const(2)),
    //       Const(3)),
    //      Const(4))))
    //}
    //"parse parens for reversed operators with precedence" in {
    //  // 1 ^ (2 * (3 + 4))
    //  val parsed = MathMLExprParser(Seq(
    //    NUMBER(1),
    //    CARET,
    //    OPENPAREN,
    //    NUMBER(2),
    //    STAR,
    //    OPENPAREN,
    //    NUMBER(3),
    //    PLUS,
    //    NUMBER(4),
    //    CLOSEPAREN,
    //    CLOSEPAREN))
    //  parsed should be(Success(
    //    Pow(
    //      Const(1),
    //      Mul(
    //        Const(2),
    //        Add(
    //          Const(3),
    //          Const(4))))))
    //}
    //"capture variable names" in {
    //  // a ^ b * c + d
    //  val parsed =
    //    Add(
    //      Mul(
    //        Pow(
    //          Var("a"),
    //          Var("b")),
    //        Var("c")),
    //      Var("d"))
    //  AstTransformer.identifiers(parsed) should be(Set(
    //    "a", "b", "c", "d"))

    //}
  //}
}
