package io.vernix

import zio.test.*
import Program.*

object TupleSpec extends ZIOSpecDefault {

  // (Int, Int, String) built from elements
  val tup: Program[Int *: Int *: String *: EmptyTuple] =
    value(1).leftEntuple(value(2).leftEntuple(value("test").leftEntuple(Program.emptyTuple)))

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
