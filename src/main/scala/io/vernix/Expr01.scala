package io.vernix

import zio.*
import zio.Console.*
import zio.interop.catz.*

object Expr01 extends ZIOAppDefault {

	override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = {
		import Program.*
		import Expr.*

		val program: Program[(Int, Int, Int)] = for {
			_ <- let("y", value(2))
			_ <- let("z", value(5))
			a <- inner {
				for {
					_ <- let("y", value(100))
					_ <- fundef[Int, Int]("double", i => value(2) * value(i))
					result <- expr(variable[Int]("y") * function[Int, Int]("double", value(3)))
				} yield result
			}
			b <- expr(variable[Int]("z") * (variable[Int]("x") + variable[Int]("y")))
			_ <- let("z", variable[Int]("y"))
			c <- expr(variable[Int]("z"))
		} yield (a, b, c)

		program[STask]
			.run(OpContext(Map("x" -> ("Int" -> 3)), Map.empty))
			.flatMap { case (ctx, result) =>
				printLine(s"Result: $result") *>
					printLine(s"Variable count: ${ctx.vars.size}") *>
					printLine(s"Function count: ${ctx.funs.size}")
			}

	}

}
