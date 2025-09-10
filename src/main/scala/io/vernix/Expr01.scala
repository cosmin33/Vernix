package io.vernix

import io.vernix.Ops.IdentState
import zio.*
import zio.Console.*
import zio.interop.catz.*
import fastparse.*

import scala.util.Try

object Expr01 extends ZIOAppDefault {

	override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = runExample1 *> runBooleanExample

	def runExample1: ZIO[ZIOAppArgs & Scope, Any, Any] = {
		import Program.*
		val e: Program[_] =
			let("x", value(2)) *>
				let("x", value(2) + value(3)) *>
				funDef[Int, Int]("triple", "i", (variable[Int]("i") * value(3))) *>
				(variable[Int]("x")
					+ variable[Int]("x")
					+ funCall[Int, Int]("triple", variable[Int]("x"))
					+ funCall[Int, Int]("triple", value(1))
					//					+ variable[Int]("i")
					)
		val ce: Task[Expr[_]] = ZIO.fromTry(e.compile)
//		val ce: Task[Expr[Int]] = ZIO.fromTry(compile(OpContext.empty)(e))
		for {
			_ <- printLine("===============================")
			_ <- printLine(s"return type:\n${e[Type].name}")
			_ <- printLine("===============================")
			_ <- printLine(s"initial code: \n\n${e[[a] =>> String]}")
			_ <- printLine("-----------------------------")
			_ <- printLine(s"initial code (with indentation): \n\n${e[IdentState].runA(0).value}")
			_ <- printLine("===============================")
			e1 <- ce
			_ <- printLine(s"compiled code: \n\n${e1[[a] =>> String]}")
			_ <- printLine("===============================")
			rr = e1.apply[Try]
			_ <- printLine(s"result: $rr")
			_ <- printLine("===============================")
			r <- e1.apply[Task]
			_ <- printLine(s"result: $r")
			_ <- printLine("===============================")
		} yield ()
	}

	def runBooleanExample: ZIO[ZIOAppArgs & Scope, Any, Any] = {
		import Program.*
		val booleanProgram: Program[Boolean] =
			let("x", value(5)) *>
				let("y", value(10)) *>
				let("isEqual", variable[Int]("x") === variable[Int]("y")) *>
				let("isGreater", variable[Int]("y") !== variable[Int]("x")) *>
				let("result", variable[Boolean]("isEqual") || variable[Boolean]("isGreater")) *>
				!variable[Boolean]("result")
		
		val ce: Task[Expr[Boolean]] = ZIO.fromTry(booleanProgram.compile)
		for {
			_ <- printLine("\n===============================")
			_ <- printLine("BOOLEAN OPERATIONS EXAMPLE")
			_ <- printLine("===============================")
			_ <- printLine(s"return type:\n${booleanProgram[Type].name}")
			_ <- printLine("===============================")
			_ <- printLine(s"initial code: \n\n${booleanProgram[[a] =>> String]}")
			_ <- printLine("-----------------------------")
			_ <- printLine(s"initial code (with indentation): \n\n${booleanProgram[IdentState].runA(0).value}")
			_ <- printLine("===============================")
			e1 <- ce
			_ <- printLine(s"compiled code: \n\n${e1[[a] =>> String]}")
			_ <- printLine("===============================")
			rr = e1.apply[Try]
			_ <- printLine(s"result: $rr")
			_ <- printLine("===============================")
			r <- e1.apply[Task]
			_ <- printLine(s"result: $r")
			_ <- printLine("===============================")
		} yield ()
	}



}
