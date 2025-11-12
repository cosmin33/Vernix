package io.vernix

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.interop.catz.*

object VariableSpec extends ZIOSpecDefault {
  def spec = suite("Variable Operations")(
    suite("Variable Declaration and Access")(
      test("add and access a variable") {
        val program = 
          Program.addVar("x", Program.value(42)) *>
          Program.variable[Int]("x")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 42)
      },
      test("add multiple variables") {
        val program = 
          Program.addVar("x", Program.value(10)) *>
          Program.addVar("y", Program.value(20)) *>
          Program.variable[Int]("y")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 20)
      },
      test("use variable in arithmetic") {
        val program = 
          Program.addVar("x", Program.value(5)) *>
          (Program.variable[Int]("x") + Program.value(3))
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 8)
      },
      test("use multiple variables in expression") {
        val program = 
          Program.addVar("x", Program.value(10)) *>
          Program.addVar("y", Program.value(20)) *>
          (Program.variable[Int]("x") + Program.variable[Int]("y"))
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 30)
      }
    ),
    suite("Variable Assignment")(
      test("set existing variable") {
        val program = 
          Program.addVar("x", Program.value(5)) *>
          Program.setVar("x", Program.value(10)) *>
          Program.variable[Int]("x")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 10)
      },
      test("set variable with expression") {
        val program = 
          Program.addVar("x", Program.value(5)) *>
          Program.setVar("x", Program.value(5) + Program.value(3)) *>
          Program.variable[Int]("x")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 8)
      },
      test("set variable using itself") {
        val program = 
          Program.addVar("counter", Program.value(1)) *>
          Program.setVar("counter", Program.variable[Int]("counter") + Program.value(1)) *>
          Program.variable[Int]("counter")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 2)
      }
    ),
    suite("Variable Scoping")(
      test("nested scope creates new scope") {
        val program = 
          Program.addVar("x", Program.value(10)) *>
          (Program.addVar("x", Program.value(20)) *> Program.variable[Int]("x")).nest *>
          Program.variable[Int]("x")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 10)
      },
      test("inner scope can access outer variable") {
        val program = 
          Program.addVar("x", Program.value(10)) *>
          (Program.variable[Int]("x") + Program.value(5)).nest
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 15)
      },
      test("inner scope modification affects outer scope") {
        val program = 
          Program.addVar("x", Program.value(10)) *>
          (Program.setVar("x", Program.value(20))).nest *>
          Program.variable[Int]("x")
        for {
          result <- program.execute[Task]()
        } yield assertTrue(result == 20)
      }
    ),
    suite("Variable Error Cases")(
      test("accessing non-existent variable fails") {
        val program = Program.variable[Int]("nonexistent")
        for {
          exit <- program.execute[Task]().exit
        } yield assertTrue(exit.isFailure)
      },
      test("setting non-existent variable fails") {
        val program = Program.setVar("nonexistent", Program.value(42))
        for {
          exit <- program.execute[Task]().exit
        } yield assertTrue(exit.isFailure)
      }
    )
  )
}
