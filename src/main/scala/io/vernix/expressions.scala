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
	def repeatUntil(condition: Expr[Boolean]): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].repeatUntil(self[F])(condition[F])
	def ifElse[B](ifTrue: Expr[B], ifFalse: Expr[B])(using e: A =:= Boolean): Expr[B] = new Expr[B]:
		def apply[F[_]: Ops]: F[B] = Ops[F].ifElse(e.substituteCo(self[F]))(ifTrue[F], ifFalse[F])
	def leftEntuple[T <: NonEmptyTuple](t: Expr[T]): Expr[A *: T] = new Expr[A *: T]:
		def apply[F[_]: Ops]: F[A *: T] = Ops[F].leftEntuple(self[F], t[F])
	def rightEntuple[T <: NonEmptyTuple](t: Expr[T]): Expr[Tuple.Append[T, A]] = new Expr[Tuple.Append[T, A]]:
		def apply[F[_]: Ops]: F[Tuple.Append[T, A]] = Ops[F].rightEntuple(t[F], self[F])
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
	def doWhile[A](condition: Expr[Boolean])(action: Expr[A]): Expr[Unit] = new Expr[Unit]:
		def apply[F[_]: Ops]: F[Unit] = Ops[F].doWhile(condition[F])(action[F])
end Expr
