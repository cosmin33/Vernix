package io.vernix

import zio.test.*

object IdentStateSpec extends ZIOSpecDefault {
  val O = Ops[IdentState]
  val S = Statements[IdentState]

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
          |  if (1 == 2) {
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
    },
    test("appending to a tuple renders it flat, like consing") {
      val pair: IdentState[(Int, Int)] =
        O.leftEntuple[Int, Int *: EmptyTuple](O.value(1), O.leftEntuple[Int, EmptyTuple](O.value(2), O.emptyTuple))
      assertTrue(O.rightEntuple[(Int, Int), String](pair, O.value("test")).runA(0).value == "(1, 2, test)")
    }
  ) + suite("Statements[IdentState] indentation")(
    test("variables and calls render inline") {
      assertTrue(
        S.variable[Int]("x").runA(0).value == "x",
        S.addVar[Int]("x", O.value(1)).runA(0).value == "var x: Int = 1",
        S.setVar[Int]("x", O.value(2)).runA(0).value == "x = 2",
        S.callFunction[Int, Int]("f", S.variable[Int]("x")).runA(0).value == "f(x)"
      )
    },
    test("nest indents its body") {
      val program = S.nest(O.*>(S.addVar[Int]("x", O.value(1)), S.variable[Int]("x")))
      val expected =
        """{
          |  var x: Int = 1
          |  x
          |}""".stripMargin
      assertTrue(program.runA(0).value == expected)
    },
    test("function body indents, including nested blocks") {
      val i = S.variable[Int]("i")
      val program =
        S.addFunction[Int, Int]("f", "i", O.ifElse(O.`>`[Int](i, O.value(0)))(i, O.neg[Int](i)))
      val expected =
        """def f(i: Int): Int = {
          |  if (i > 0) {
          |    i
          |  } else {
          |    (-i)
          |  }
          |}""".stripMargin
      assertTrue(program.runA(0).value == expected)
    },
    test("a Program renders with indentation") {
      import Program.*
      val program: Program[Int] =
        addVar("x", value(2)) *>
          addFunction[Int, Int]("triple", "i", variable[Int]("i") * value(3)) *>
          whileDo(variable[Int]("x") < value(10))(setVar("x", callFunction[Int, Int]("triple", variable[Int]("x")))) *>
          variable[Int]("x")
      val expected =
        """var x: Int = 2
          |def triple(i: Int): Int = {
          |  (i * 3)
          |}
          |while {
          |  (x < 10)
          |} do {
          |  x = triple(x)
          |}
          |x""".stripMargin
      assertTrue(program.apply[IdentState].runA(0).value == expected)
    }
  )
}
