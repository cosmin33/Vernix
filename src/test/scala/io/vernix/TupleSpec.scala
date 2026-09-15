package io.vernix

import zio.test.*
import Program.*

object TupleSpec extends ZIOSpecDefault {

  // (Int, Int, String) built from elements
  val tup: Program[Int *: Int *: String *: EmptyTuple] =
    value(1).leftEntuple(value(2).leftEntuple(value("test").leftEntuple(Program.emptyTuple)))

  // The same (Int, Int, String), built by appending the last element instead of consing it
  val appended: Program[(Int, Int, String)] =
    value("test").rightEntuple(value(1).leftEntuple(value(2).leftEntuple(Program.emptyTuple)))

  def spec = suite("Tuples")(
    suite("Type names")(
      test("2-tuple") {
        assertTrue(Type[(Int, String)].name == "(Int, String)")
      },
      test("3-tuple") {
        assertTrue(Type[(Int, Int, String)].name == "(Int, Int, String)")
      },
      test("nested tuple") {
        assertTrue(Type[(Int, (String, Boolean))].name == "(Int, (String, Boolean))")
      },
      test("empty tuple") {
        assertTrue(Type[EmptyTuple].name == "EmptyTuple")
      }
    ),
    suite("DSL construction and access")(
      test("whole tuple evaluates to the runtime tuple") {
        assertTrue(tup.evaluate() == Right((1, 2, "test")))
      },
      test("at(index) accessor") {
        assertTrue(
          tup.at(0).evaluate() == Right(1),
          tup.at(1).evaluate() == Right(2),
          tup.at(2).evaluate() == Right("test")
        )
      },
      test("tuple type name reported by the Type interpreter") {
        assertTrue(tup.apply[Type].name == "(Int, Int, String)")
      }
    ),
    suite("appending (rightEntuple)")(
      test("appending names the same type as consing") {
        assertTrue(
          appended.apply[Type].name == tup.apply[Type].name,
          appended.apply[Type].name == Type[(Int, Int, String)].name
        )
      },
      test("appending twice stays normalised") {
        val twice: Program[(Int, Int, String, Boolean)] = value(true).rightEntuple(appended)
        assertTrue(twice.apply[Type].name == "(Int, Int, String, Boolean)")
      },
      test("an appended tuple type resolves and destructures") {
        assertTrue(
          Type.resolve(appended.apply[Type].name).map(_.name) == Some("(Int, Int, String)"),
          Type.elementTypes(appended.apply[Type]).map(_.map(_.name)) == Some(List("Int", "Int", "String"))
        )
      },
      test("appending renders flat, like consing") {
        assertTrue(
          appended.apply[IdentState].runA(0).value == "(1, 2, test)",
          appended.apply[IdentState].runA(0).value == tup.apply[IdentState].runA(0).value
        )
      },
      // Only reachable when a caller forges a `Type` from a name (`ProgramCodec` does); statically
      // `Append` reduces and `NonEmptyTupleType` names the result instead.
      test("appending to an unparenthesised type name leaves it intact") {
        val forged = new Type[NonEmptyTuple] { def name = "EmptyTuple" }
        assertTrue(Type.TupRightType[NonEmptyTuple, Int](using forged, Type[Int]).name == "(EmptyTuple, Int)")
      },
      test("appended tuple evaluates and reads back") {
        assertTrue(
          appended.evaluate() == Right((1, 2, "test")),
          appended.at(2).evaluate() == Right("test")
        )
      },
      // Unascribed, so `NonEmptyTupleType` and `TupRightType` both match `Tuple.Append[(Int, Int), String]`
      // and the call only compiles while the latter stays deprioritised.
      test("an appended type is unambiguous at a Type call site") {
        def nameOf[A: Type](p: Program[A]): String = Type[A].name
        val p = value("test").rightEntuple(value(1).leftEntuple(value(2).leftEntuple(Program.emptyTuple)))
        assertTrue(nameOf(p) == "(Int, Int, String)")
      },
      // The case `TupRightType` alone can serve: `Append` cannot reduce, so it must stay a given.
      test("appending is still typed when the tuple type is abstract") {
        def appendName[T <: NonEmptyTuple: Type, A: Type]: String = Type[Tuple.Append[T, A]].name
        assertTrue(appendName[(Int, Int), String] == "(Int, Int, String)")
      }
    ),
    suite("dynamic builders (parser layer)")(
      test("Program.tuple builds the runtime tuple") {
        val parts: List[Program[?]] = List(value(1), value(2), value("test"))
        assertTrue(Program.tuple(parts).evaluate() == Right((1, 2, "test")))
      },
      test("Program.tupleAt reads an element with the given element type") {
        val t = Program.tuple(List(value(1), value(2), value("test")))
        assertTrue(
          Program.tupleAt(t, 0, Type[Int]).evaluate() == Right(1),
          Program.tupleAt(t, 2, Type[String]).evaluate() == Right("test")
        )
      }
    )
  )
}
