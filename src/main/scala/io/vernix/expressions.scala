package io.vernix

import scala.compiletime.uninitialized

trait Expr[A]:
	self =>
	def apply[F[_]: Ops]: F[A]
	def len(using e: A =:= String): Expr[Int] = new Expr[Int]:
		def apply[F[_]: Ops]: F[Int] = Ops[F].len(e.substituteCo(self[F]))
	def +(that: Expr[A])(using Type[A], Numeric[A]): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].add(self[F], that[F])
	def -(that: Expr[A])(using Type[A], Numeric[A]): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].sub(self[F], that[F])
	def *(that: Expr[A])(using Type[A], Numeric[A]): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].mul(self[F], that[F])
	def /(that: Expr[A])(using Type[A], Fractional[A]): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].div(self[F], that[F])
	def quot(that: Expr[A])(using Type[A], Integral[A]): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].quot(self[F], that[F])
	def %(that: Expr[A])(using Type[A], Integral[A]): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].mod(self[F], that[F])
	def neg(using Type[A], Numeric[A]): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].neg(self[F])
	def abs(using Type[A], Numeric[A]): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].abs(self[F])
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
		@volatile var last: Any = uninitialized
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
	def prg: Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = self[F]
object Expr:
	def value[A: Type](v: A): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].value(v)
	def doWhile[A](condition: Expr[Boolean])(action: Expr[A]): Expr[Unit] = new Expr[Unit]:
		def apply[F[_]: Ops]: F[Unit] = Ops[F].doWhile(condition[F])(action[F])
end Expr

trait Blah1[+A]:
	def apply[F[+_]](using o: Ops[F]): F[A]

trait Blah2[+A] extends Blah1[A]:
	def apply[F[+_]](using o: Statements[F]): F[A]
