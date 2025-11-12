package io.vernix

import zio.*
import zio.test.*
import zio.test.Assertion.*

object OpContextSpec extends ZIOSpecDefault {
  def spec = suite("OpContext")(
    suite("Variable Management")(
      test("add variable to empty context") {
        val ctx = OpContext.empty.addVariable[Int]("x", 42)
        ctx.getVariable[Int]("x") match {
          case OpContext.SearchResult.Found(value) => assertTrue(value == 42)
          case _ => assertTrue(false)
        }
      },
      test("add multiple variables") {
        val ctx = OpContext.empty
          .addVariable[Int]("x", 10)
          .addVariable[String]("name", "Vernix")
        
        val xResult = ctx.getVariable[Int]("x")
        val nameResult = ctx.getVariable[String]("name")
        
        assertTrue(
          xResult == OpContext.SearchResult.Found(10) &&
          nameResult == OpContext.SearchResult.Found("Vernix")
        )
      },
      test("get non-existent variable returns NotFound") {
        val ctx = OpContext.empty
        ctx.getVariable[Int]("missing") match {
          case OpContext.SearchResult.NotFound => assertTrue(true)
          case _ => assertTrue(false)
        }
      },
      test("get variable with wrong type returns TypeMismatch") {
        val ctx = OpContext.empty.addVariable[Int]("x", 42)
        ctx.getVariable[String]("x") match {
          case OpContext.SearchResult.TypeMismatch(expected, found) => 
            assertTrue(expected == "String" && found == "Int")
          case _ => assertTrue(false)
        }
      },
      test("cannot add duplicate variable in same scope") {
        try {
          val ctx = OpContext.empty
            .addVariable[Int]("x", 10)
            .addVariable[Int]("x", 20)
          assertTrue(false) // Should not reach here
        } catch {
          case _: IllegalArgumentException => assertTrue(true)
          case _ => assertTrue(false)
        }
      },
      test("set existing variable") {
        val ctx = OpContext.empty.addVariable[Int]("x", 10)
        ctx.setVariable[Int]("x", 20)
        ctx.getVariable[Int]("x") match {
          case OpContext.SearchResult.Found(value) => assertTrue(value == 20)
          case _ => assertTrue(false)
        }
      },
      test("set non-existent variable throws exception") {
        val ctx = OpContext.empty
        try {
          ctx.setVariable[Int]("missing", 42)
          assertTrue(false) // Should not reach here
        } catch {
          case _: NoSuchElementException => assertTrue(true)
          case _ => assertTrue(false)
        }
      },
      test("set variable with wrong type throws exception") {
        val ctx = OpContext.empty.addVariable[Int]("x", 42)
        try {
          ctx.setVariable[String]("x", "wrong")
          assertTrue(false) // Should not reach here
        } catch {
          case _: ClassCastException => assertTrue(true)
          case _ => assertTrue(false)
        }
      }
    ),
    suite("Variable Existence Check")(
      test("exists returns true for existing variable with correct type") {
        val ctx = OpContext.empty.addVariable[Int]("x", 42)
        assertTrue(ctx.exists[Int]("x"))
      },
      test("exists returns false for non-existent variable") {
        val ctx = OpContext.empty
        assertTrue(!ctx.exists[Int]("missing"))
      },
      test("exists returns false for wrong type") {
        val ctx = OpContext.empty.addVariable[Int]("x", 42)
        assertTrue(!ctx.exists[String]("x"))
      }
    ),
    suite("Scope Management")(
      test("nest creates new scope") {
        val ctx = OpContext.empty.addVariable[Int]("x", 10)
        val nested = ctx.nest
        val result = nested.addVariable[Int]("y", 20)
        assertTrue(result.exists[Int]("x") && result.exists[Int]("y"))
      },
      test("unnest removes inner scope") {
        val ctx = OpContext.empty
          .addVariable[Int]("x", 10)
          .nest
          .addVariable[Int]("y", 20)
        val unnested = ctx.unNest
        assertTrue(unnested.exists[Int]("x") && !unnested.exists[Int]("y"))
      },
      test("can shadow variables in nested scope") {
        val ctx = OpContext.empty
          .addVariable[Int]("x", 10)
          .nest
          .addVariable[Int]("x", 20)
        
        ctx.getVariable[Int]("x") match {
          case OpContext.SearchResult.Found(value) => assertTrue(value == 20)
          case _ => assertTrue(false)
        }
      },
      test("modifications in nested scope affect outer scope") {
        val ctx = OpContext.empty.addVariable[Int]("x", 10)
        val nested = ctx.nest
        nested.setVariable[Int]("x", 20)
        
        ctx.getVariable[Int]("x") match {
          case OpContext.SearchResult.Found(value) => assertTrue(value == 20)
          case _ => assertTrue(false)
        }
      },
      test("unnest from single scope returns same scope") {
        val ctx = OpContext.empty.addVariable[Int]("x", 10)
        val unnested = ctx.unNest
        assertTrue(unnested.exists[Int]("x"))
      }
    ),
    suite("Multiple Scope Levels")(
      test("access variable from multiple levels deep") {
        val ctx = OpContext.empty
          .addVariable[Int]("x", 10)
          .nest
          .addVariable[Int]("y", 20)
          .nest
          .addVariable[Int]("z", 30)
        
        assertTrue(
          ctx.exists[Int]("x") &&
          ctx.exists[Int]("y") &&
          ctx.exists[Int]("z")
        )
      },
      test("shadowing works through multiple levels") {
        val ctx = OpContext.empty
          .addVariable[Int]("x", 10)
          .nest
          .addVariable[Int]("x", 20)
          .nest
          .addVariable[Int]("x", 30)
        
        ctx.getVariable[Int]("x") match {
          case OpContext.SearchResult.Found(value) => assertTrue(value == 30)
          case _ => assertTrue(false)
        }
      }
    )
  )
}
