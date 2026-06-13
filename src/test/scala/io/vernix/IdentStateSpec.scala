package io.vernix

import zio.test.*
import Ops.IdentState

object IdentStateSpec extends ZIOSpecDefault {
  val O = Ops[IdentState]

  def spec = suite("Ops[IdentState] indentation")(
    test("values and inline expressions have no stray indentation") {
      assertTrue(
        O.value(42).runA(0).value == "42",
        O.add[Int](O.value(1), O.value(2)).runA(0).value == "(1 + 2)"
      )
    },
    test("one level of nesting") {
      val program = O.ifElse(O.value(true))(O.value(1), O.value(2))
      val expected =
        """if (true) {
          |  1
          |} else {
          |  2
          |}""".stripMargin
      assertTrue(program.runA(0).value == expected)
    },
    test("two levels of nesting") {
      val inner = O.ifElse(O.value(true))(O.value(1), O.value(2))
      val program = O.ifElse(O.value(false))(inner, O.value(3))
      val expected =
        """if (false) {
          |  if (true) {
          |    1
          |  } else {
          |    2
          |  }
          |} else {
          |  3
          |}""".stripMargin
      assertTrue(program.runA(0).value == expected)
    },
    test("multi-statement block bodies indent each statement") {
      val program = O.whileDo(O.value(true))(O.*>(O.value(1), O.value(2)))
      val expected =
        """while {
          |  true
          |} do {
          |  1
          |  2
          |}""".stripMargin
      assertTrue(program.runA(0).value == expected)
    },
    test("three levels of nesting with mixed constructs") {
      val program =
        O.*>(
          O.value(0),
          O.whileDo(O.`<`[Int](O.value(1), O.value(10)))(
            O.ifElse(O.equals[Int](O.value(1), O.value(2)))(
              O.repeatUntil(O.value(5))(O.value(true)),
              O.value(7)
            )
          )
        )
      val expected =
        """0
          |while {
          |  (1 < 10)
          |} do {
          |  if ((1 == 2)) {
          |    repeat {
          |      5
          |    } until {
          |      true
          |    }
          |  } else {
          |    7
          |  }
          |}""".stripMargin
      assertTrue(program.runA(0).value == expected)
    }
  )
}
