package io.vernix

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*
import zio.test.*
import Program.*

object JsonCodecSpec extends ZIOSpecDefault {

  /** Encode, decode and run, so the assertion covers the whole round trip. */
  def roundTrip[A: {Encoder, Decoder, Type}](p: Program[A]): Either[String, A] =
    p.asJson.as[Program[A]].left.map(_.getMessage).flatMap(_.evaluate().left.map(_.getMessage))

  val tup: Program[Int *: String *: EmptyTuple] =
    value(1).leftEntuple(value("a").leftEntuple(Program.emptyTuple))

  def tupleAt(t: Json, elemType: String, index: Int): Json =
    Json.obj(
      "op" -> Json.fromString("tuple-at"),
      "type" -> Json.fromString(elemType),
      "t" -> t,
      "index" -> Json.fromInt(index)
    )

  def spec = suite("ProgramCodec")(
    suite("Arithmetic")(
      test("nested arithmetic") {
        assertTrue(roundTrip(value(5) + value(3) * value(2)) == Right(11))
      },
      test("integer division and remainder") {
        assertTrue(roundTrip(value(7).quot(value(2)) + value(7) % value(2)) == Right(4))
      },
      test("fractional division") {
        assertTrue(roundTrip(value(7.0) / value(2.0)) == Right(3.5))
      },
      test("neg and abs") {
        assertTrue(roundTrip(value(5).neg.abs) == Right(5))
      },
      test("toDouble") {
        assertTrue(roundTrip(value(3).toDouble / value(2.0)) == Right(1.5))
      }
    ),
    suite("Strings and booleans")(
      test("concat and len") {
        assertTrue(roundTrip((value("ab") ++ value("cde")).len) == Right(5))
      },
      test("comparisons and boolean operators") {
        assertTrue(roundTrip((value(1) < value(2)) && !(value("a") === value("b"))) == Right(true))
      },
      test("ifElse") {
        assertTrue(roundTrip((value(3) >= value(3)).ifElse(value("yes"), value("no"))) == Right("yes"))
      }
    ),
    suite("Statements")(
      test("addVar, setVar and variable") {
        val p = addVar("x", value(5)) *> setVar("x", variable[Int]("x") + value(1)) *> variable[Int]("x")
        assertTrue(roundTrip(p) == Right(6))
      },
      test("nest") {
        assertTrue(roundTrip((addVar("y", value(2)) *> variable[Int]("y")).nest) == Right(2))
      },
      test("function definition and call") {
        val p = addFunction[Int, Int]("dbl", "n", variable[Int]("n") * value(2)) *> callFunction[Int, Int]("dbl", value(21))
        assertTrue(roundTrip(p) == Right(42))
      },
      test("whileDo") {
        val p = addVar("i", value(0)) *> whileDo(variable[Int]("i") < value(3))(setVar("i", variable[Int]("i") + value(1))) *> variable[Int]("i")
        assertTrue(roundTrip(p) == Right(3))
      },
      test("repeatUntil") {
        val p = addVar("j", value(0)) *> Program.repeatUntil(setVar("j", variable[Int]("j") + value(1)) *> variable[Int]("j"))(variable[Int]("j") === value(2))
        assertTrue(roundTrip(p) == Right(2))
      }
    ),
    suite("Tuples")(
      test("built tuple and element access") {
        assertTrue(roundTrip(tup.at(1) ++ value("!")) == Right("a!"))
      },
      test("tuple literal") {
        assertTrue(roundTrip(value((1, "a")).at(0)) == Right(1))
      },
      test("rightEntuple") {
        assertTrue(roundTrip(value("z").rightEntuple(tup).at(2)) == Right("z"))
      },
      test("rightEntuple as the result type") {
        assertTrue(roundTrip(value("z").rightEntuple(tup)) == Right((1, "a", "z")))
      }
    ),
    suite("Literals of the program's own type")(
      test("a type only the supplied codec knows round-trips") {
        assertTrue(roundTrip(value(Option(1))) == Right(Some(1)))
      },
      test("a literal no codec covers encodes to a node that fails to decode") {
        val p: Program[Int] = value(Option(1)) *> value(1)
        assertTrue(p.asJson.as[Program[Int]].left.map(_.message) == Left("Literal of type Option[Int] was not encodable"))
      }
    ),
    suite("JSON shape")(
      test("a literal carries its type") {
        assertTrue(value(1).asJson == Json.obj("op" -> Json.fromString("value"), "type" -> Json.fromString("Int"), "v" -> Json.fromInt(1)))
      },
      test("an operator node is derived from its operands") {
        val expected = Json.obj(
          "op" -> Json.fromString("add"),
          "l" -> value(1).asJson,
          "r" -> Program.variable[Int]("x").asJson
        )
        assertTrue((value(1) + Program.variable[Int]("x")).asJson == expected)
      }
    ),
    suite("Failures")(
      test("decoding at the wrong type is rejected") {
        val decoded = value(1).asJson.as[Program[String]]
        assertTrue(decoded.left.map(_.message) == Left("Program is of type Int, not String"))
      },
      test("an unknown operation is rejected") {
        val decoded = Json.obj("op" -> Json.fromString("nope")).as[Program[Int]]
        assertTrue(decoded.left.map(_.message) == Left("Unknown operation: nope"))
      },
      test("an ill-typed node is rejected") {
        val bad = Json.obj("op" -> Json.fromString("add"), "l" -> value(1).asJson, "r" -> value("a").asJson)
        assertTrue(bad.as[Program[Int]].isLeft)
      },
      // Hand-written tuple nodes: the encoder never emits these, so only the decoder's checks stand
      // between them and a `Program[Int]` that evaluates to something else entirely.
      test("tupleAt with the wrong element type is rejected") {
        val bad = tupleAt(value((1, "a")).asJson, "Int", 1)
        assertTrue(bad.as[Program[Int]].isLeft)
      },
      test("tupleAt out of range is rejected") {
        assertTrue(
          tupleAt(value((1, "a")).asJson, "Int", 7).as[Program[Int]].isLeft,
          tupleAt(value((1, "a")).asJson, "Int", -1).as[Program[Int]].isLeft
        )
      },
      test("tupleAt on a non-tuple is rejected") {
        assertTrue(tupleAt(value(1).asJson, "Int", 0).as[Program[Int]].isLeft)
      },
      // Sequenced, so the result type is the sibling's: nothing but the entupling checks themselves
      // stops a node with a nonsense tuple type from surviving the decode.
      test("entupling a non-tuple is rejected") {
        def seq(node: Json) = Json.obj("op" -> Json.fromString("seq"), "l" -> node, "r" -> value(0).asJson)
        val snoc = Json.obj("op" -> Json.fromString("right-entuple"), "t" -> value(1).asJson, "a" -> value(2).asJson)
        val cons = Json.obj("op" -> Json.fromString("left-entuple"), "a" -> value(1).asJson, "t" -> value(2).asJson)
        assertTrue(
          seq(snoc).as[Program[Int]].isLeft,
          seq(cons).as[Program[Int]].isLeft
        )
      }
    )
  )
}
