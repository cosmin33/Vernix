package io.vernix

import cats.data.State

/** Pretty-printing interpreter: the state is the current indentation, the value the rendered text. */
type IdentState[A] = State[Int, String]
object IdentState:
	extension (s: String)
		def ident(n: Int): String = (" " * n) + s
	/** Renders `body` as a `{ ... }` block at indentation `i`. A body that already is a `nest` block keeps
	  * its own braces instead of getting a second pair. */
	def block(body: IdentState[String], i: Int): String =
		val rendered = body.runA(i).value
		if rendered.startsWith("{\n") && rendered.endsWith("\n" + "}".ident(i)) then rendered
		else "{" + "\n" + body.runA(i + 2).value.ident(i + 2) + "\n" + "}".ident(i)
