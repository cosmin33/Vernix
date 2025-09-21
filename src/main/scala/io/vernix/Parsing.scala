package io.vernix

import scala.util.Try
import fastparse.*
import fastparse.MultiLineWhitespace.*

object Parsing {

	case class NamedFunction(f: Char => Boolean)(using name: sourcecode.Name) extends (Char => Boolean):
		def apply(t: Char) = f(t)
		override def toString() = name.value

	val isOpChar = NamedFunction(c => "+-*/|&".contains(c))
	val LetterDigitDollarUnderscore = NamedFunction(c => isLetter(c) | isDigit(c) | c == '$' | c == '_')
	val LowerChar = NamedFunction(c => isLower(c) || c == '$' | c == '_')
	val UpperChar = NamedFunction(isUpper)

	private def isLetter(c: Char) = Character.isLetter(c)
	private def isDigit(c: Char) = Character.isDigit(c)
	private def isLower(c: Char) = Character.isLowerCase((c))
	private def isUpper(c: Char) = Character.isUpperCase(c)

	private def Lower[$: P] = P(CharPred(LowerChar))
	private def Upper[$: P] = P(CharPred(UpperChar))

	private def W[$: P](s: String) = P( s ~ !CharPred(LetterDigitDollarUnderscore))(using s"`$s`", summon)

	private def Digit[$: P]    = P(CharIn("0-9"))
	private def HexDigit[$: P] = P(CharIn("0-9a-fA-F"))
	private def HexNum[$: P]   = P("0x" ~ CharsWhileIn("0-9a-fA-F"))
	private def DecNum[$: P]   = P(CharsWhileIn("0-9"))

	private def WSChars[$: P] = P(NoTrace(CharsWhileIn("\u0020\u0009")))
	private def Newline[$: P] = P(NoTrace(StringIn("\r\n", "\n")))
	private def Semi[$: P] = P(";" | Newline.rep(1))
	private def OpChar[$: P] = P(CharPred(isOpChar))

	def parseUnknown(s: String): Try[Program[?]] = {
		def `let`   [$: P] = W("var")
		def `funDef`[$: P] = W("def")
		def `if`    [$: P] = W("if")
		def `then`  [$: P] = W("then")
		def `else`  [$: P] = W("else")
		def `while` [$: P] = W("while")
		def `do`    [$: P] = W("do")
		def `repeat`[$: P] = W("repeat")
		def `until` [$: P] = W("until")


		def `int`[$: P]: P[Program[Int]] = P(CharIn("+\\-").?.! ~~ DecNum).!.map(s => Program.value(s.toInt))
		def `double`[$: P]: P[Program[Double]] = P(CharIn("+\\-").?.! ~~ DecNum ~~ ("." ~~ DecNum).? ~~ "d".?).!.map(s => Program.value(s.toDouble))
		def parens[$: P]: P[Program[Int]] = P("(" ~/ addSub ~ ")")
		def factor[$: P]: P[Program[Int]] = P(`int` | parens)
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

		def numberValue[$: P]: P[Program[? >: Double & Int <: AnyVal]] = P(`int` | `double`)

		def program[$: P]: P[Program[?]] = P(addSub ~ End)

		println("Assertions......")
		println("==============================")
		val Parsed.Success(value, successIndex) = parse("var", `let`(using _)).get
		assert(value == () & successIndex == 3)
		println("==============================")
		val x0 = parse("-3 - 1 - 1", program(using _))
		x0 match
			case f: Parsed.Failure => println(s"msg: ${f.msg}, trace: ${f.trace().longMsg}")
			case s @ Parsed.Success(value, index) => println(s"Parsed: \"\"\"\n${value[[a] =>> String]}\n\"\", value: ${value.compile.flatMap(_[Try])}, index: $index")
		println("==============================")
		val x1: Parsed[Program[?]] = parse("-2 + (1 + 2) * 3", program(using _))
		x1 match
			case f: Parsed.Failure => println(s"msg: ${f.msg}, trace: ${f.trace().longMsg}")
			case s @ Parsed.Success(value, index) => println(s"Parsed: \"\"\"\n${value[[a] =>> String]}\n\"\"\", value: ${value.compile.flatMap(_[Try])}, index: $index")
		println("==============================")
		val x2 = parse("-44.2134d", `double`(using _))
		x2 match
			case f: Parsed.Failure => println(s"msg: ${f.msg}, trace: ${f.trace().longMsg}")
			case s @ Parsed.Success(value, index) => println(s"Parsed: \"\"\"\n${value[[a] =>> String]}\n\"\"\", value: ${value.compile.flatMap(_[Try])}, index: $index")
		println("==============================")

		Try(???)

	}

	def main(args: Array[String]): Unit = {
		val r = parseUnknown("var x = 2 + 3")
		println("-------------------------------")
		println(r)
		println("-------------------------------")
	}

}
