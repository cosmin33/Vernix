package io.vernix

import zio.*
import zio.Console.*
import zio.interop.catz.*

import scala.util.Try

object Expr01 extends ZIOAppDefault {

	override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = {

		val e: Program[Int] =
			Program.let("x", Program.value(2) + Program.value(3)) *>
				Program.funDef[Int, Int]("triple", "i", Program.variable[Int]("i") * Program.value(3)) *>
				(Program.variable[Int]("x")
					+ Program.variable[Int]("x")
					+ Program.function[Int, Int]("triple", Program.variable[Int]("x"))
					+ Program.function[Int, Int]("triple", Program.value(1))
					//					+ Program.variable[Int]("i")
					)
		val magarie: Program[_] = e
		val ce: Task[Expr[Int]] = ZIO.fromTry(Program.compile(OpContext.empty)(e))
		for {
			_ <- printLine("===============================")
			_ <- printLine(s"initial code: \n\n${e[[a] =>> String]}")
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
