package io.vernix

import zio.*
import zio.test.*
import zio.test.Assertion.*

object VarHeapSpec extends ZIOSpecDefault {
  def spec = suite("VarHeap")(
    suite("Variable Management")(
      test("add variable to empty heap") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 42)
        val result = heap.getVariable[Int]("x")
        assertTrue(result == 42)
      },
      test("add multiple variables") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 10)
        heap.addVariable[String]("name", "Vernix")
        
        assertTrue(
          heap.getVariable[Int]("x") == 10 &&
          heap.getVariable[String]("name") == "Vernix"
        )
      },
      test("get non-existent variable throws exception") {
        val heap = VarHeap.empty
        try {
          heap.getVariable[Int]("missing")
          assertTrue(false) // Should not reach here
        } catch {
          case _: NoSuchElementException => assertTrue(true)
          case _ => assertTrue(false)
        }
      },
      test("get variable with wrong type throws exception") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 42)
        try {
          heap.getVariable[String]("x")
          assertTrue(false) // Should not reach here
        } catch {
          case _: ClassCastException => assertTrue(true)
          case _ => assertTrue(false)
        }
      },
      test("cannot add duplicate variable in same scope") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 10)
        try {
          heap.addVariable[Int]("x", 20)
          assertTrue(false) // Should not reach here
        } catch {
          case _: IllegalArgumentException => assertTrue(true)
          case _ => assertTrue(false)
        }
      },
      test("set existing variable mutates value") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 10)
        heap.setVariable[Int]("x", 20)
        assertTrue(heap.getVariable[Int]("x") == 20)
      },
      test("set non-existent variable throws exception") {
        val heap = VarHeap.empty
        try {
          heap.setVariable[Int]("missing", 42)
          assertTrue(false) // Should not reach here
        } catch {
          case _: NoSuchElementException => assertTrue(true)
          case _ => assertTrue(false)
        }
      },
      test("set variable with wrong type throws exception") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 42)
        try {
          heap.setVariable[String]("x", "wrong")
          assertTrue(false) // Should not reach here
        } catch {
          case _: ClassCastException => assertTrue(true)
          case _ => assertTrue(false)
        }
      }
    ),
    suite("Scope Management")(
      test("nest creates new scope") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 10)
        val nested = heap.nest()
        nested.addVariable[Int]("y", 20)
        assertTrue(
          nested.getVariable[Int]("x") == 10 &&
          nested.getVariable[Int]("y") == 20
        )
      },
      test("unnest removes inner scope") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 10)
        val nested = heap.nest()
        nested.addVariable[Int]("y", 20)
        val unnested = nested.unNest()
        
        assertTrue(unnested.getVariable[Int]("x") == 10)
        try {
          unnested.getVariable[Int]("y")
          assertTrue(false) // Should not reach here
        } catch {
          case _: NoSuchElementException => assertTrue(true)
          case _ => assertTrue(false)
        }
      },
      test("can shadow variables in nested scope") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 10)
        val nested = heap.nest()
        nested.addVariable[Int]("x", 20)
        
        assertTrue(nested.getVariable[Int]("x") == 20)
      },
      test("modifications in nested scope affect outer scope for outer variables") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 10)
        val nested = heap.nest()
        nested.setVariable[Int]("x", 20)
        
        assertTrue(heap.getVariable[Int]("x") == 20)
      },
      test("unnest from single scope returns same scope") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 10)
        val unnested = heap.unNest()
        assertTrue(unnested.getVariable[Int]("x") == 10)
      }
    ),
    suite("Multiple Scope Levels")(
      test("access variable from multiple levels deep") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 10)
        val level1 = heap.nest()
        level1.addVariable[Int]("y", 20)
        val level2 = level1.nest()
        level2.addVariable[Int]("z", 30)
        
        assertTrue(
          level2.getVariable[Int]("x") == 10 &&
          level2.getVariable[Int]("y") == 20 &&
          level2.getVariable[Int]("z") == 30
        )
      },
      test("shadowing works through multiple levels") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 10)
        val level1 = heap.nest()
        level1.addVariable[Int]("x", 20)
        val level2 = level1.nest()
        level2.addVariable[Int]("x", 30)
        
        assertTrue(level2.getVariable[Int]("x") == 30)
      },
      test("unnesting restores previous scope variables") {
        val heap = VarHeap.empty
        heap.addVariable[Int]("x", 10)
        val level1 = heap.nest()
        level1.addVariable[Int]("x", 20)
        val level2 = level1.nest()
        level2.addVariable[Int]("x", 30)
        
        val backToLevel1 = level2.unNest()
        assertTrue(backToLevel1.getVariable[Int]("x") == 20)
      }
    )
  )
}
