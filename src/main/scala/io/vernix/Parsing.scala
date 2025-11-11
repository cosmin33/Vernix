package io.vernix

import scala.util.Try
import fastparse.*
import fastparse.ScalaWhitespace.*
import zio.interop.catz.*
import Prog.prog
import cats.Eval
import cats.data.EitherT

object Parsing {

	case class NamedFunction(f: Char => Boolean)(using name: sourcecode.Name) extends (Char => Boolean):
		def apply(t: Char) = f(t)
		override def toString() = name.value

	val isOpChar = NamedFunction(c => "+-*/|&".contains(c))
	val LetterDollarUnderscore = NamedFunction(c => isLetter(c) | c == '$' | c == '_')
	val LetterDigitDollarUnderscore = NamedFunction(c => isLetter(c) | isDigit(c) | c == '$' | c == '_')
	val LowerChar = NamedFunction(c => isLower(c) || c == '$' | c == '_')
	val UpperChar = NamedFunction(isUpper)

	private def isLetter(c: Char) = Character.isLetter(c)
	private def isDigit(c: Char) = Character.isDigit(c)
	private def isLower(c: Char) = Character.isLowerCase((c))
	private def isUpper(c: Char) = Character.isUpperCase(c)

	private def Lower[$: P] = P(CharPred(LowerChar))
	private def Upper[$: P] = P(CharPred(UpperChar))

	private def validName[$: P] = P(CharPred(LetterDollarUnderscore) ~~ CharPred(LetterDigitDollarUnderscore).rep).!

	private def W[$: P](s: String) = P(s ~~ !CharPred(LetterDigitDollarUnderscore)) //(using s"`$s`", summon)

	private def Digit[$: P]    = P(CharIn("0-9"))
	private def HexDigit[$: P] = P(CharIn("0-9a-fA-F"))
	private def HexNum[$: P]   = P("0x" ~ CharsWhileIn("0-9a-fA-F"))
	private def DecNum[$: P]   = P(CharsWhileIn("0-9"))

	private def WSChars[$: P] = P(NoTrace(CharsWhileIn("\u0020\u0009")))
	private def Newline[$: P] = P(NoTrace(StringIn("\r\n", "\n")))
	private def Semi[$: P] = P(";" | Newline.rep(1))
	private def OpChar[$: P] = P(CharPred(isOpChar))

	def parseUnknown(s: String): Either[String, Program[?]] = {
		def `let`   [$: P] = W("var")
		def `funDef`[$: P] = W("def")
		def `if`    [$: P] = W("if")
		def `then`  [$: P] = W("then")
		def `else`  [$: P] = W("else")
		def `while` [$: P] = W("while")
		def `do`    [$: P] = W("do")
		def `repeat`[$: P] = W("repeat")
		def `until` [$: P] = W("until")


		def `int`[$: P]: P[Prog.Aux[Int]] = P(CharIn("+\\-").?.! ~~ DecNum ~~ !CharIn(".d")).!.map(s => Program.value(s.toInt).prog)
		def `double`[$: P]: P[Prog.Aux[Double]] =
			P(CharIn("+\\-").?.! ~~ DecNum ~~ ((("." ~~ DecNum) ~~ "d".?) | (("." ~~ DecNum).? ~~ "d"))).!
				.map(s => Program.value(s.toDouble).prog)
		def `true` [$: P]: P[Prog.Aux[Boolean]] = P(W("true")).map(_ => Program.value(true).prog)
		def `false`[$: P]: P[Prog.Aux[Boolean]] = P(W("false")).map(_ => Program.value(false).prog)
		def boolean[$: P]: P[Prog.Aux[Boolean]] = P(`true` | `false`)
		def variable[$: P, T](using Type[T]): P[Prog.Aux[T]] = P(validName).map(name => Program.variable[T](name).prog)

		import ParsingOps.*

		def parens[$: P, T]: P[Prog] = P("(" ~/ andOr ~ ")")
		def factor[$: P]: P[Prog] = P(`int` | `double` | boolean | parens | block | variableInt)
		def variableInt[$: P]: P[Prog] = P(validName).map:
			name =>
				Program.variable[Int](name).prog
		def divMul[$: P, T]: P[Prog] =
			Try(P(factor ~ (CharIn("*/%").! ~/ factor).rep).map {
				case (left: Prog, ops: Seq[(String, Prog)]) =>
					ops.foldLeft(left):
						case (acc, ("*", r)) => op_*(acc, r)
						case (acc, ("/", r)) => op_/(acc, r)
						case (acc, ("%", r)) => op_%(acc, r)
						case _ => throw new Exception("unreachable")
			}).fold(t => P(Fail(t.getMessage)), identity)

		def additionSubtraction[$: P]: P[Prog] =
			Try(P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map {
				case (left: Prog, ops: Seq[(String, Prog)]) =>
					ops.foldLeft(left):
						case (acc, ("+", r)) => op_+(acc, r)
						case (acc, ("-", r)) => op_-(acc, r)
						case _ => throw new Exception("unreachable")
			}).fold(t => P(Fail(t.getMessage)), identity)

		def comparison[$: P]: P[Prog] =
			Try(P(additionSubtraction ~ (("<=" | "<" | ">=" | ">" | "==" | "!=").! ~/ additionSubtraction).rep).map {
				case (left: Prog, ops: Seq[(String, Prog)]) =>
					ops.foldLeft(left):
						case (acc, ("<", r))  => op_<(acc, r)
						case (acc, ("<=", r)) => op_<=(acc, r)
						case (acc, (">", r))  => op_>(acc, r)
						case (acc, (">=", r)) => op_>=(acc, r)
						case (acc, ("==", r)) => op_===(acc, r)
						case (acc, ("!=", r)) => op_!==(acc, r)
						case _ => throw new Exception("unreachable")
			}).fold(t => P(Fail(t.getMessage)), identity)

		def andOr[$: P]: P[Prog] =
			Try(P(comparison ~ (CharIn("|&").! ~/ comparison).rep).map {
				case (left: Prog, ops: Seq[(String, Prog)]) =>
					ops.foldLeft(left):
						case (acc, ("&", r)) => op_&(acc, r)
						case (acc, ("|", r)) => op_|(acc, r)
						case _ => throw new Exception("unreachable")
			}).fold(t => P(Fail(t.getMessage)), identity)

		def letStmt[$: P]: P[Prog] =
			P(`let` ~ validName ~ "=" ~ statement).map:
				case (name, value) =>
					given Type[value.T] = value.`type`
					Program.addVar(name, value.program).prog

		def assignStmt[$: P]: P[Prog] =
			P(validName ~ "=" ~ statement).map:
				case (name, value) =>
					given Type[value.T] = value.`type`
					Program.setVar(name, value.program).prog

		def ifStmt[$: P]: P[Prog] =
			Try(P(`if` ~ statement ~ `then`.? ~ statement ~ `else` ~ statement).map:
				case (cond, thenp, elsep) => opIf(cond, thenp, elsep)
			).fold(t => P(Fail(t.getMessage)), identity)

		def repeatUntil[$: P]: P[Prog] =
			Try(P(`repeat` ~ statement ~ `until` ~ statement).map:
				case (action, condition) =>
					Program.repeatUntil(action.program)(condition.unsafe[Boolean]).prog(using action.`type`)
			).fold(t => P(Fail(t.getMessage)), identity)

		def whileDo[$: P]: P[Prog] =
			Try(P(`while` ~ statement ~ `do` ~ statement).map:
				case (condition, action) => Program.whileDo(condition.unsafe[Boolean])(action.program).prog
			).fold(t => P(Fail(t.getMessage)), identity)

		def statement[$: P]: P[Prog] = P(ifStmt | repeatUntil | whileDo | letStmt | assignStmt | andOr)

		def block[$: P]: P[Prog] = P("{" ~ statement ~ (Semi.? ~ statement).rep ~ "}").map:
			case (first: Prog, rest: Seq[Prog]) =>
				rest.foldLeft(first)((acc, p) => acc.program.*>(p.program).prog(using p.`type`)).nest

		def prog[$: P]: P[Prog] = P(Semi.? ~ statement ~ (Semi.? ~ statement).rep ~ End).map:
			case (first: Prog, rest: Seq[Prog]) =>
				rest.foldLeft(first)((acc, p) => acc.program.*>(p.program).prog(using p.`type`))

		def program[$: P]: P[Program[?]] = prog.map(_.program)

		parse(s, program(using _)) match
			case f: Parsed.Failure => Left(f.trace().longMsg)
			case s @ Parsed.Success(value, index) => Right(value)
	}

	type EvalErr[A] = EitherT[Eval, Throwable, A]

	def parseRun(s: String): Unit = {
		val r = parseUnknown(s)
		println("-------------------------------")
		println(r.map(_[[a] =>> String]))
		println("===============================")
		println(r.map(_.execute[EvalErr]().value.value))
		println("-------------------------------")
	}

	def main(args: Array[String]): Unit = {
		parseRun(
			"""var x = 1
				|x
				|""".stripMargin
		)
		parseRun(
			"""
				var x = {2 + 3 * 4}; var y = x - 5 / 2
				x = x - 10
				if y > 10 then x = x + 10 else x = x - 10
				while x < 20 do
				  x = x + 2
				repeat
				  x = x + 3
				until x >= 30
	 			x
			"""
		)
		parseRun(
			"""
				|var x = {var y = 1; y + 1}
				|x = x + 2
				|x
				|""".stripMargin
		)
		parseRun(
			"""var x = 1
				|while x < 10 do
				|  x = x + 1
				|x
				|""".stripMargin
		)

	}

}
