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
}
