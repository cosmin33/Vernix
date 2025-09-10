package io.vernix

import scala.util.Try
import fastparse.*

object Parsing {

	def parseProgram[A](s: String)(using T: Type[A]): Try[Program[A]] =
		Try {
			val p: Program[?] = parseUnknown(s).get
			T match {
				case value: Program[A] => value
				case _ => throw new Exception(s"Parsed program is not of expected type Program[${Type[A].name}]")
			}
		}

	def parseUnknown(s: String): Try[Program[?]] = {
		def `let`   [$: P] = P("let")
		def `funDef`[$: P] = P("def")
		def `if`    [$: P] = P("if")
		def `then`  [$: P] = P("then")
		def `else`  [$: P] = P("else")
		def `while` [$: P] = P("while")
		def `do`    [$: P] = P("do")
		def `repeat`[$: P] = P("repeat")
		def `until` [$: P] = P("until")

		import fastparse.MultiLineWhitespace.*

		def number[$: P]: P[Program[Int]] = P(CharIn("0-9").rep(1).!.map(s => Program.value(s.toInt)))
		def parens[$: P]: P[Program[Int]] = P("(" ~/ program ~ ")")
		def factor[$: P]: P[Program[Int]] = P(number | parens)
		def divMul[$: P]: P[Program[Int]] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map:
			case (left: Program[Int], ops: Seq[(String, Program[Int])]) =>
				ops.foldLeft(left):
					case (acc, ("*", r)) => acc * r
					case (acc, ("/", r)) => acc.quot(r)
					case (_, (_, _)) => throw new Exception("unreachable")

		def addSub[$: P]: P[Program[Int]] =
			P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map:
				case (left: Program[Int], ops: Seq[(String, Program[Int])]) =>
					ops.foldLeft(left):
						case (acc, ("+", r)) => acc + r
						case (acc, ("-", r)) => acc - r
						case (_, (_, _)) => throw new Exception("unreachable")

		def program[$: P]: P[Program[Int]] = P(addSub ~ End)

		println("Assertions......")
		println("==============================")
		val Parsed.Success(value, successIndex) = parse("let", `let`(using _)).get
		assert(value == (), successIndex == 1)
		println("==============================")

		Try(throw new NotImplementedError())

	}

	def main(args: Array[String]): Unit = {
		val r = parseUnknown("let x = 2 + 3")
		println(r)
	}

}
