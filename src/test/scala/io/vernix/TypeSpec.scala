package io.vernix

import zio.*
import zio.test.*
import zio.test.Assertion.*
import Program.*

object TypeSpec extends ZIOSpecDefault {
  def spec = suite("Type System")(
    suite("Basic Types")(
      test("Unit type has correct name") {
        val tpe = Type[Unit]
        assertTrue(tpe.name == "Unit")
      },
      test("Int type has correct name") {
        val tpe = Type[Int]
        assertTrue(tpe.name == "Int")
      },
      test("String type has correct name") {
        val tpe = Type[String]
        assertTrue(tpe.name == "String")
      },
      test("Boolean type has correct name") {
        val tpe = Type[Boolean]
        assertTrue(tpe.name == "Boolean")
      },
      test("Double type has correct name") {
        val tpe = Type[Double]
        assertTrue(tpe.name == "Double")
      }
    ),
    suite("Complex Types")(
      test("Function type has correct name") {
        val tpe = Type[Int => String]
        assertTrue(tpe.name == "(Int => String)")
      },
      test("Tuple2 type has correct name") {
        val tpe = Type[(Int, String)]
        assertTrue(tpe.name == "(Int, String)")
      },
      test("Either type has correct name") {
        val tpe = Type[Either[Int, String]]
        assertTrue(tpe.name == "Either[Int, String]")
      },
      test("List type has correct name") {
        val tpe = Type[List[Int]]
        assertTrue(tpe.name == "List[Int]")
      },
      test("Array type has correct name") {
        val tpe = Type[Array[String]]
        assertTrue(tpe.name == "Array[String]")
      },
      test("Option type has correct name") {
        val tpe = Type[Option[Int]]
        assertTrue(tpe.name == "Option[Int]")
      }
    ),
    suite("Nested Types")(
      test("nested List type has correct name") {
        val tpe = Type[List[List[Int]]]
        assertTrue(tpe.name == "List[List[Int]]")
      },
      test("nested Option type has correct name") {
        val tpe = Type[Option[Option[String]]]
        assertTrue(tpe.name == "Option[Option[String]]")
      }
    ),
    suite("Type Inference in Program")(
      test("Program with Int value has Int type") {
        val program = value(42)
        val tpe = program.apply[Type]
        assertTrue(tpe.name == "Int")
      },
      test("Program with String value has String type") {
        val program = value("Hello")
        val tpe = program.apply[Type]
        assertTrue(tpe.name == "String")
      },
      test("Program with Boolean value has Boolean type") {
        val program = value(true)
        val tpe = program.apply[Type]
        assertTrue(tpe.name == "Boolean")
      },
      test("Program with arithmetic operation has correct type") {
        val program = value(10) + value(5)
        val tpe = program.apply[Type]
        assertTrue(tpe.name == "Int")
      },
      test("Program with comparison has Boolean type") {
        val program = value(10) > value(5)
        val tpe = program.apply[Type]
        assertTrue(tpe.name == "Boolean")
      },
      test("Program with string concatenation has String type") {
        val program = value("Hello") ++ value("World")
        val tpe = program.apply[Type]
        assertTrue(tpe.name == "String")
      }
    )
  )
}
