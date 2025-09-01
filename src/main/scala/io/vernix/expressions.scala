package io.vernix

trait Expr[A]:
	self =>
	def apply[F[_]: Ops]: F[A]
	def len(using e: A =:= String): Expr[Int] = new Expr[Int]:
		def apply[F[_]: Ops]: F[Int] = Ops[F].len(e.substituteCo(self[F]))
	def +(that: Expr[Int])(using e: A =:= Int): Expr[Int] = new Expr[Int]:
		def apply[F[_]: Ops]: F[Int] = Ops[F].add(e.substituteCo(self[F]), that[F])
	def *(that: Expr[Int])(using e: A =:= Int): Expr[Int] = new Expr[Int]:
		def apply[F[_]: Ops]: F[Int] = Ops[F].mul(e.substituteCo(self[F]), that[F])
	def ++(that: Expr[String])(using e: A =:= String): Expr[String] = new Expr[String]:
		def apply[F[_]: Ops]: F[String] = Ops[F].concat(e.substituteCo(self[F]), that[F])
	def memoize: Expr[A] = new Expr[A]:
		@volatile var last: Any = _
		@volatile var lastType: String = ""
		def apply[F[_]: Ops]: F[A] = synchronized {
			if (lastType == Ops[F].typeK.name)
				last.asInstanceOf[F[A]]
			else
				val r = self[F]
				last = r
				lastType = Ops[F].typeK.name
				r
		}
object Expr:
	def value[A](v: A): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].value(v)
end Expr
