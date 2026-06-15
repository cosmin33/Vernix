package io.vernix

import zio.test.*

object ParsingSpec extends ZIOSpecDefault {

  /** Parse and evaluate, returning the value on success. Fails the test on parse/eval error. */
  private def run(src: String): Any =
    Parsing.parseUnknown(src) match
      case Left(err) => throw new AssertionError(s"parse failed: $err")
      case Right(p) =>
        p.evaluate() match
          case Left(err) => throw new AssertionError(s"evaluation failed: $err", err)
          case Right(v)  => v

  /** Parse and evaluate, exposing the runtime Either (for testing runtime errors). */
  private def eval(src: String): Either[Throwable, ?] =
    Parsing.parseUnknown(src) match
      case Left(err) => throw new AssertionError(s"parse failed: $err")
      case Right(p)  => p.evaluate()

  /** The String pretty-printer interpreter applied to a parsed program. */
  private def render(src: String): String =
    Parsing.parseUnknown(src) match
      case Left(err) => throw new AssertionError(s"parse failed: $err")
      case Right(p)  => p.apply[[a] =>> String]

  private def parseFails(src: String): Boolean = Parsing.parseUnknown(src).isLeft

  def spec = suite("Parsing")(
    suite("literals")(
      test("int")(assertTrue(run("42") == 42)),
      test("negative int")(assertTrue(run("-7") == -7)),
      test("double")(assertTrue(run("3.5") == 3.5)),
      test("double with d suffix")(assertTrue(run("1d") == 1.0)),
      test("true")(assertTrue(run("true") == true)),
      test("false")(assertTrue(run("false") == false)),
      test("string")(assertTrue(run("\"hello\"") == "hello")),
      test("empty string")(assertTrue(run("\"\"") == ""))
    ),
    suite("arithmetic")(
      test("addition")(assertTrue(run("2 + 3") == 5)),
      test("subtraction")(assertTrue(run("10 - 4") == 6)),
      test("multiplication")(assertTrue(run("6 * 7") == 42)),
      test("integer division")(assertTrue(run("7 / 2") == 3)),
      test("modulo")(assertTrue(run("7 % 3") == 1)),
      test("precedence: * before +")(assertTrue(run("2 + 3 * 4") == 14)),
      test("parentheses override precedence")(assertTrue(run("(2 + 3) * 4") == 20)),
      test("nested parentheses")(assertTrue(run("((1 + 2) * (3 + 4))") == 21)),
      test("double division")(assertTrue(run("7.0 / 2.0") == 3.5)),
      test("int/double coercion")(assertTrue(run("1 + 2.0") == 3.0)),
      test("double/int coercion")(assertTrue(run("5.0 - 1") == 4.0))
    ),
    suite("strings")(
      test("concatenation via +")(assertTrue(run("\"foo\" + \"bar\"") == "foobar")),
      test("equality")(assertTrue(run("\"a\" == \"a\"") == true)),
      test("inequality")(assertTrue(run("\"a\" != \"b\"") == true))
    ),
    suite("comparison")(
      test("less than")(assertTrue(run("3 < 5") == true)),
      test("less or equal")(assertTrue(run("5 <= 5") == true)),
      test("greater than")(assertTrue(run("7 > 2") == true)),
      test("greater or equal")(assertTrue(run("2 >= 7") == false)),
      test("equals")(assertTrue(run("4 == 4") == true)),
      test("not equals")(assertTrue(run("4 != 5") == true)),
      test("mixed int/double comparison")(assertTrue(run("3 < 3.5") == true))
    ),
    suite("boolean")(
      test("and")(assertTrue(run("true & false") == false)),
      test("or")(assertTrue(run("true | false") == true)),
      test("comparison combined with and")(assertTrue(run("1 < 2 & 3 < 4") == true))
    ),
    suite("variables")(
      test("declare and read")(assertTrue(run("var x = 5\nx") == 5)),
      test("declare and use in expression")(assertTrue(run("var x = 5\nx + 1") == 6)),
      test("reassignment")(assertTrue(run("var x = 1\nx = 10\nx") == 10)),
      test("assignment using previous value")(assertTrue(run("var x = 1\nx = x + 2\nx") == 3)),
      test("multiple variables")(assertTrue(run("var x = 2\nvar y = 3\nx * y") == 6)),
      test("semicolon separated statements")(assertTrue(run("var x = 2; var y = 3; x + y") == 5)),
      test("block expression as initializer")(assertTrue(run("var x = {var y = 1; y + 1}\nx = x + 2\nx") == 4))
    ),
    suite("scoping")(
      test("inner block shadows outer variable without affecting it") {
        assertTrue(run("var x = 1d\n{\n  var x = 2\n  x = 3\n}\nx") == 1.0)
      }
    ),
    suite("control flow")(
      test("if-then-else true branch")(assertTrue(run("var x = 1\nif x < 5 then x = 10 else x = 20\nx") == 10)),
      test("if-then-else false branch")(assertTrue(run("var x = 8\nif x < 5 then x = 10 else x = 20\nx") == 20)),
      test("while loop")(assertTrue(run("var x = 0\nwhile x < 5 do x = x + 1\nx") == 5)),
      test("repeat-until loop")(assertTrue(run("var x = 0\nrepeat x = x + 3 until x >= 9\nx") == 9)),
      test("repeat runs at least once")(assertTrue(run("var x = 100\nrepeat x = x + 1 until x >= 9\nx") == 101))
    ),
    suite("functions")(
      test("define and call")(assertTrue(run("def triple(x: Int) = x * 3\ntriple(5)") == 15)),
      test("function using argument multiple times")(assertTrue(run("def sum3(a: Int) = a + a + a\nsum3(4)") == 12)),
      test("call with expression argument")(assertTrue(run("def inc(x: Int) = x + 1\ninc(2 * 3)") == 7)),
      test("variable defined before function is usable as argument")(assertTrue(run("var a = 2 + 3\ndef triple(a: Int) = a + a + a\na = 1\ntriple(a)") == 3))
    ),
    suite("tuples")(
      test("tuple literal evaluates to runtime tuple")(assertTrue(run("(1, 2, \"test\")") == (1, 2, "test"))),
      test("two-element tuple")(assertTrue(run("(10, 20)") == (10, 20))),
      test("accessor on literal")(assertTrue(run("(1, 2, 3)._2") == 2)),
      test("destructuring binds each element (int)")(assertTrue(run("var (i, d, s) = (1, 2, \"test\")\ni") == 1)),
      test("destructuring binds each element (string)")(assertTrue(run("var (i, d, s) = (1, 2, \"test\")\ns") == "test")),
      test("tuple-typed variable then accessor")(assertTrue(run("var t = (1, 2, \"test\")\nt._3") == "test")),
      test("accessed elements used in arithmetic")(assertTrue(run("var t = (10, 20)\nt._1 + t._2") == 30)),
      test("single parenthesised value is grouping, not a tuple")(assertTrue(run("(1 + 2) * 3") == 9)),
      test("nested tuple access")(assertTrue(run("var t = (1, (2, 3))\nt._2._1") == 2)),
      test("mixed-type tuple destructuring with computed elements")(assertTrue(run("var x = 5\nvar (a, b) = (x + 1, \"v\")\na") == 6))
    ),
    suite("error cases")(
      test("undefined variable")(assertTrue(parseFails("y"))),
      test("type mismatch on assignment")(assertTrue(parseFails("var x = 5\nx = 2.0\nx"))),
      test("function called with wrong argument type")(assertTrue(parseFails("def f(x: Int) = x\nf(true)"))),
      test("destructuring arity mismatch")(assertTrue(parseFails("var (a, b) = (1, 2, 3)\na"))),
      test("accessor on non-tuple")(assertTrue(parseFails("var x = 5\nx._1"))),
      test("unterminated parenthesis")(assertTrue(parseFails("(1 + 2")))
    ),
    suite("string interpreter rendering")(
      test("variable declaration renders with type") {
        assertTrue(render("var x = 1\nx") == "var x: Int = 1\nx")
      },
      test("arithmetic renders with explicit parentheses") {
        assertTrue(render("2 + 3 * 4") == "(2 + (3 * 4))")
      }
    ),
    // Three small classical programs that, combined, exercise every Vernix construct:
    // function def/call, blocks, var/assignment, while, repeat/until, if/then/else,
    // arithmetic (+ - * / %), int/double coercion, comparisons (< <= > >= == !=),
    // boolean (& |), true/false, string literals/concat/comparison, and tuples
    // (literal, destructuring, accessor).
    suite("classical programs")(
      test("factorial and triangular number, returned as a destructured tuple => 135") {
        // fact(5) = 120, triangular(5) = 5*6/2 = 15, f + t = 135.
        // Covers: def/call, block body, var, while/do, <=, *, +, /, tuple literal, destructuring.
        val program =
          """
            |def fact(n: Int) = {
            |  var acc = 1
            |  var i = 1
            |  while i <= n do {
            |    acc = acc * i
            |    i = i + 1
            |  }
            |  acc
            |}
            |var n = 5
            |var result = (fact(n), n * (n + 1) / 2)
            |var (f, t) = result
            |f + t
            |""".stripMargin
        assertTrue(run(program) == 135)
      },
      test("gcd via Euclid's algorithm, then lcm - gcd => 132") {
        // gcd(48, 36) = 12, lcm = 48*36/12 = 144, lcm - gcd = 132.
        // Covers: while/do, !=, %, -, *, /, nested block, assignment to outer-scope vars.
        val program =
          """
            |var a = 48
            |var b = 36
            |var x = a
            |var y = b
            |while y != 0 do {
            |  var t = y
            |  y = x % y
            |  x = t
            |}
            |var g = x
            |var l = a * b / g
            |l - g
            |""".stripMargin
        assertTrue(run(program) == 132)
      },
      test("sign labelling with strings, booleans, doubles, a loop and a tuple => 4.5") {
        // s = "pos/nonpos"; matches = true; isOk = true; avg = (4.0 + 5) / 2d = 4.5;
        // count counts up to 3; data._3 selects avg = 4.5.
        // Covers: def returning String, if/then/else, negative int, string +/==, & |,
        // true/false, double literal, d-suffix double, int/double coercion, repeat/until,
        // > >= <, 4-element tuple literal, accessor ._3.
        val program =
          """
            |def label(n: Int) = if n > 0 then "pos" else "nonpos"
            |var s = label(5) + "/" + label(-3)
            |var matches = s == "pos/nonpos"
            |var isOk = matches & true | false
            |var avg = (4.0 + 5) / 2d
            |var count = 0
            |repeat count = count + 1 until count >= 3
            |var data = (s, isOk, avg, count)
            |data._3
            |""".stripMargin
        assertTrue(run(program) == 4.5)
      }
    )
  )
}
