package io.vernix

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.interop.catz.*
import Program.*

object VariableSpec extends ZIOSpecDefault {
  def spec = suite("Variable Operations")(
    suite("Variable Declaration and Access")(
      test("add and access a variable") {
        val program = 
          addVar("x", value(42)) *>
          variable[Int]("x")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 42)
      },
      test("add multiple variables") {
        val program = 
          addVar("x", value(10)) *>
          addVar("y", value(20)) *>
          variable[Int]("y")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 20)
      },
      test("use variable in arithmetic") {
        val program = 
          addVar("x", value(5)) *>
          (variable[Int]("x") + value(3))
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 8)
      },
      test("use multiple variables in expression") {
        val program = 
          addVar("x", value(10)) *>
          addVar("y", value(20)) *>
          (variable[Int]("x") + variable[Int]("y"))
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 30)
      }
    ),
    suite("Variable Assignment")(
      test("set existing variable") {
        val program = 
          addVar("x", value(5)) *>
          setVar("x", value(10)) *>
          variable[Int]("x")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 10)
      },
      test("set variable with expression") {
        val program = 
          addVar("x", value(5)) *>
          setVar("x", value(5) + value(3)) *>
          variable[Int]("x")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 8)
      },
      test("set variable using itself") {
        val program = 
          addVar("counter", value(1)) *>
          setVar("counter", variable[Int]("counter") + value(1)) *>
          variable[Int]("counter")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 2)
      }
    ),
    suite("Variable Scoping")(
      test("nested scope creates new scope") {
        val program = 
          addVar("x", value(10)) *>
          (addVar("x", value(20)) *> variable[Int]("x")).nest *>
          variable[Int]("x")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 10)
      },
      test("inner scope can access outer variable") {
        val program = 
          addVar("x", value(10)) *>
          (variable[Int]("x") + value(5)).nest
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 15)
      },
      test("inner scope modification affects outer scope") {
        val program = 
          addVar("x", value(10)) *>
          (setVar("x", value(20))).nest *>
          variable[Int]("x")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 20)
      }
    ),
    suite("Variable Error Cases")(
      test("accessing non-existent variable fails") {
        val program = variable[Int]("nonexistent")
        for {
          exit <- program.execute[Task]().exit
        } yield assertTrue(exit.isFailure)
      },
      test("setting non-existent variable fails") {
        val program = setVar("nonexistent", value(42))
        for {
          exit <- program.execute[Task]().exit
        } yield assertTrue(exit.isFailure)
      }
    )
  )
}
