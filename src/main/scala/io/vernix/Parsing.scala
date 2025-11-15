package io.vernix

import cats.Eval
import cats.data.EitherT
import fastparse.*
import fastparse.ScalaWhitespace.*
import io.vernix.Prog.prog
import scala.util.Try

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
		var ctx: VarCtx = VarCtx.empty
		val strToType: Map[String, Type[?]] =
			Map(
				"Int"     -> Type.IntType,
				"Double"  -> Type.DoubleType,
				"Boolean" -> Type.BooleanType,
				"String"	-> Type.StringType,
			)

		def Fail(msg: String)(implicit ctx: P[?]): Nothing =
			throw new Exception(s"Parsing failure at index ${ctx.index}: $msg")

		def `let`   [$: P] = W("var")
		def `def`   [$: P] = W("def")
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
		def variableName[$: P, T: Type]: P[Prog.Aux[T]] = P(validName).map(name => Program.variable[T](name).prog)

		import ParsingOps.*

		def parens[$: P, T]: P[Prog] = P("(" ~/ andOr ~ ")")
		def factor[$: P]: P[Prog] = P(`int` | `double` | boolean | parens | block | variable | funCall)
		def variable[$: P]: P[Prog] = P(validName ~ !CharIn("(=")).flatMapX:
			name =>
				ctx.getVariableType(name) match
					case Some(tpe) =>
						strToType.get(tpe) match
							case Some(t) => Pass(Program.variable[t.TypeOf](name)(using t).prog(using t))
							case None    => Fail(s"Unsupported variable type: $tpe")
					case None        => Fail(s"Undefined variable: $name")

		def divMul[$: P, T]: P[Prog] =
			P(factor ~ (CharIn("*/%").! ~/ factor).rep).map {
				case (left: Prog, ops: Seq[(String, Prog)]) =>
					ops.foldLeft(left):
						case (acc, ("*", r)) => op_*(acc, r)
						case (acc, ("/", r)) => op_/(acc, r)
						case (acc, ("%", r)) => op_%(acc, r)
						case _ => throw new Exception("unreachable")
			}

		def additionSubtraction[$: P]: P[Prog] =
			P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map {
				case (left: Prog, ops: Seq[(String, Prog)]) =>
					ops.foldLeft(left):
						case (acc, ("+", r)) => op_+(acc, r)
						case (acc, ("-", r)) => op_-(acc, r)
						case _ => throw new Exception("unreachable")
			}

		def comparison[$: P]: P[Prog] =
			P(additionSubtraction ~ (("<=" | "<" | ">=" | ">" | "==" | "!=").! ~/ additionSubtraction).rep).map {
				case (left: Prog, ops: Seq[(String, Prog)]) =>
					ops.foldLeft(left):
						case (acc, ("<", r))  => op_<(acc, r)
						case (acc, ("<=", r)) => op_<=(acc, r)
						case (acc, (">", r))  => op_>(acc, r)
						case (acc, (">=", r)) => op_>=(acc, r)
						case (acc, ("==", r)) => op_===(acc, r)
						case (acc, ("!=", r)) => op_!==(acc, r)
						case _ => throw new Exception("unreachable")
			}

		def andOr[$: P]: P[Prog] =
			P(comparison ~ (CharIn("|&").! ~/ comparison).rep).map {
				case (left: Prog, ops: Seq[(String, Prog)]) =>
					ops.foldLeft(left):
						case (acc, ("&", r)) => op_&(acc, r)
						case (acc, ("|", r)) => op_|(acc, r)
						case _ => throw new Exception("unreachable")
			}

		def letStmt[$: P]: P[Prog] =
			P(`let` ~ validName ~ "=" ~ statement).flatMap:
				case (name, value) =>
					given Type[value.T] = value.`type`
					Try(ctx.addVariable(name, value.`type`.name)) match
						case scala.util.Success(_) => Pass(Program.addVar(name, value.program).prog)
						case scala.util.Failure(ex) => Fail(ex.getMessage)

		def assignStmt[$: P]: P[Prog] =
			P(validName ~ "=" ~ statement).flatMap:
				case (name, value) =>
					given Type[value.T] = value.`type`
					ctx.getVariableType(name) match
						case Some(tpe) =>
							strToType.get(tpe) match
								case Some(t) if t == value.`type` =>
									Pass(Program.setVar(name, value.program).prog)
								case Some(_) => Fail(s"Type mismatch in assignment to $name: expected $tpe, got ${value.`type`.name}")
								case None    => Fail(s"Unsupported variable type: $tpe")
						case None    => Fail(s"Undefined variable: $name")

		def funInterface[$: P]: P[(String, String, String)] =
			P(`def` ~ validName ~ "(" ~ validName ~ ":" ~ validName ~ ")").flatMap:
				case (funName, argName, argType) =>
					ctx = ctx.nest()
					Try(ctx.addVariable(argName, argType)) match
						case scala.util.Success(_) => Pass((funName, argName, argType))
						case scala.util.Failure(ex) => Fail(ex.getMessage)


		def funDef[$: P]: P[Prog] =
			P(funInterface ~ "=" ~ statement).flatMap:
				case (funName, argName, argType, body) =>
					strToType.get(argType) match
						case Some(t) =>
							given Type[body.T] = body.`type`
							given Type[t.TypeOf] = t
							Try(ctx.addVariable(funName, s"${argType} => ${body.`type`.name}")) match
								case scala.util.Success(_) =>
									Pass(Program.addFunction[t.TypeOf, body.T](funName, argName, body.program).prog)
								case scala.util.Failure(ex) => Fail(ex.getMessage)
						case None => Fail(s"Unsupported function argument type: $argType")
				.flatMap {p => ctx.unNest(); Pass(p)}

		def funCall[$: P]: P[Prog] =
			P(validName ~ "(" ~ statement ~ ")").flatMap:
				case (funName, arg) =>
					ctx.getVariableType(funName) match
						case Some(tpe) if tpe.contains("=>") =>
							val Array(argType, returnType) = tpe.split("=>").map(_.trim)
							strToType.get(argType) match
								case Some(at) if at == arg.`type` =>
									strToType.get(returnType) match
										case Some(rt) =>
											val a = arg.program.asInstanceOf[Program[at.TypeOf]] // safe due to the type check above
											P.current.map(_ => Program.callFunction[at.TypeOf, rt.TypeOf](funName, a)(using at, rt).prog(using rt))
										case None => Fail(s"Unsupported function return type: $returnType")
								case Some(_) => Fail(s"Function $funName expects argument of type $argType, got ${arg.`type`.name}")
								case None => Fail(s"Unsupported function argument type: $argType")
						case Some(_) => Fail(s"$funName is not a function")
						case None    => Fail(s"Undefined function: $funName")

		def ifStmt[$: P]: P[Prog] =
			P(`if` ~ statement ~ `then`.? ~ statement ~ `else` ~ statement).map:
				case (cond, thenp, elsep) => opIf(cond, thenp, elsep)

		def repeatUntil[$: P]: P[Prog] =
			P(`repeat` ~ statement ~ `until` ~ statement).map:
				case (action, condition) =>
					Program.repeatUntil(action.program)(condition.unsafe[Boolean]).prog(using action.`type`)

		def whileDo[$: P]: P[Prog] =
			P(`while` ~ statement ~ `do` ~ statement).map:
				case (condition, action) => Program.whileDo(condition.unsafe[Boolean])(action.program).prog

		def statement[$: P]: P[Prog] = P(ifStmt | repeatUntil | whileDo | letStmt | assignStmt | funDef | andOr)

		def doNesting[A](b: => A): A =
			ctx = ctx.nest()
			val result = b
			ctx = ctx.unNest()
			result

		def block[$: P]: P[Prog] = doNesting(
			P("{" ~ statement ~ (Semi.? ~ statement).rep ~ "}").map:
				case (first: Prog, rest: Seq[Prog]) =>
					rest.foldLeft(first)((acc, p) => acc.program.*>(p.program).prog(using p.`type`)).nest
		)

		def prog[$: P]: P[Prog] = P(Semi.? ~ statement ~ (Semi.? ~ statement).rep ~ End).map:
			case (first: Prog, rest: Seq[Prog]) =>
				rest.foldLeft(first)((acc, p) => acc.program.*>(p.program).prog(using p.`type`))

		def program[$: P]: P[Program[?]] = prog.map(_.program)

		Try(parse(s, program(using _), verboseFailures = true) match
			case f: Parsed.Failure => Left(f.longMsg)
			case s @ Parsed.Success(value, index) => Right(value)
		).toEither.left.map(_.getMessage).flatten
	}

	type EvalErr[A] = EitherT[Eval, Throwable, A]

	def parsePrint(s: String): Unit = {
		val r: Either[String, Program[?]] = parseUnknown(s)
		println("-------------------------------")
		println(r.map(_[[a] =>> String]))
		println("===============================")
		println(r.map(_.evaluate()))
		//val task: Either[String, Task[?]] = r.map(_.execute[Task]())
		println("-------------------------------")
	}

	def main(args: Array[String]): Unit = {
		doStuff()
	}

	def doStuff(): Unit = {
		parsePrint(
			"""var x = 1 / 0
				|x
				|""".stripMargin
		)
		parsePrint(
			"""
				var x = {2 + 3 * 4}; var y = x - 5 / 2
				x = x - 10
				if y > 10 then x = x + 10 else x = x - 10
				while x < 20 do
				  x = x + 2
				repeat
				  x = x + 3
				until x >= 30
				def triple(x: Int) = x * 3
				x = x + triple(5)
	 			x
			"""
		)
		parsePrint(
			"""
				|var x = {var y = 1; y + 1}
				|x = x + 2
				|x
				|""".stripMargin
		)
		parsePrint(
			"""var x = 1d
				|{
				|  var x = 2
				|  x = 3
				|}
				|x
				|""".stripMargin
		)
		parsePrint(
			"""
				|var a = 2 + 3
				|def triple(a: Int) = a + a + a
				|a = 1
				|triple(a)
				|""".stripMargin
		)
	}

}
