package io.vernix

import cats.data.State

/** Pretty-printing interpreter: the state is the current indentation, the value the rendered text. */
type IdentState[A] = State[Int, String]
object IdentState:
	extension (s: String)
		def ident(n: Int): String = (" " * n) + s
