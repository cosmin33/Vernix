package io.vernix

import cats.data.State
import cats.implicits.*
import cats.{Apply, Monad}

trait Ops[F[_]]:
	def value[A: Type](v: A): F[A]
	def add[N: {Type, Numeric}](l: F[N], r: F[N]): F[N]
	def sub[N: {Type, Numeric}](l: F[N], r: F[N]): F[N]
	def mul[N: {Type, Numeric}](l: F[N], r: F[N]): F[N]
	def div[N: {Type, Fractional}](l: F[N], r: F[N]): F[N]
	def quot[N: {Type, Integral}](l: F[N], r: F[N]): F[N]
	def mod[N: {Type, Integral}](l: F[N], r: F[N]): F[N]
	def neg[N: {Type, Numeric}](a: F[N]): F[N]
	def abs[N: {Type, Numeric}](a: F[N]): F[N]
	def concat(l: F[String], r: F[String]): F[String]
	def len(fa:F[String]): F[Int]
	def toDouble(fa: F[Int]): F[Double]
	def repeatUntil[A](action: => F[A])(condition: => F[Boolean]): F[A]
	def whileDo[A](condition: => F[Boolean])(action: => F[A]): F[Unit]
	def ifElse[A](cond: F[Boolean])(ifTrue: => F[A], ifFalse: => F[A]): F[A]
	def and(l: F[Boolean], r: F[Boolean]): F[Boolean]
	def or(l: F[Boolean], r: F[Boolean]): F[Boolean]
	def not(a: F[Boolean]): F[Boolean]
	def equals[A: Type](l: F[A], r: F[A]): F[Boolean]
	def notEquals[A: Type](l: F[A], r: F[A]): F[Boolean]
	def < [A: {Type, Ordering}](l: F[A], r: F[A]): F[Boolean]
	def <=[A: {Type, Ordering}](l: F[A], r: F[A]): F[Boolean]
	def > [A: {Type, Ordering}](l: F[A], r: F[A]): F[Boolean]
	def >=[A: {Type, Ordering}](l: F[A], r: F[A]): F[Boolean]
	def leftEntuple[A, T <: NonEmptyTuple](a: F[A], t: F[T]): F[A *: T]
	def rightEntuple[T <: NonEmptyTuple, A](t: F[T], a: F[A]): F[Tuple.Append[T, A]]
	def *>[A, B](l: F[A], r: F[B]): F[B]
object Ops:
	def apply[F[_]](using ops: Ops[F]): Ops[F] = ops

	given[F[_]: Monad]: Ops[F] =
		new Ops[F]:
			def value[A: Type](v: A): F[A] = Monad[F].pure(v)
			def add[N: {Type, Numeric}](l: F[N], r: F[N]): F[N] = Apply[F].map2(l, r)(Numeric[N].plus)
			def sub[N: {Type, Numeric}](l: F[N], r: F[N]): F[N] = Apply[F].map2(l, r)(Numeric[N].minus)
			def mul[N: {Type, Numeric}](l: F[N], r: F[N]): F[N] = Apply[F].map2(l, r)(Numeric[N].times)
			def div[N: {Type, Fractional}](l: F[N], r: F[N]): F[N] = Apply[F].map2(l, r)(Fractional[N].div)
			def quot[N: {Type, Integral}](l: F[N], r: F[N]): F[N] = Apply[F].map2(l, r)(Integral[N].quot)
			def mod[N: {Type, Integral}](l: F[N], r: F[N]): F[N] = Apply[F].map2(l, r)(Integral[N].rem)
			def neg[N: {Type, Numeric}](a: F[N]): F[N] = a.map(Numeric[N].negate)
			def abs[N: {Type, Numeric}](a: F[N]): F[N] = a.map(Numeric[N].abs)
			def len(fa:F[String]): F[Int] = fa.map(_.length)
			def toDouble(fa: F[Int]): F[Double] = fa.map(_.toDouble)
			def concat(l: F[String], r: F[String]): F[String] = Apply[F].map2(l, r)(_ + _)
			def repeatUntil[A](action: => F[A])(condition: => F[Boolean]): F[A] =
				action >>= (a => condition >>= (if _ then Monad[F].pure(a) else repeatUntil(action)(condition)))
			def whileDo[A](condition: => F[Boolean])(action: => F[A]): F[Unit] =
				condition >>= (if _ then action *> whileDo(condition)(action) else Monad[F].pure(()))
			def ifElse[A](cond: F[Boolean])(ifTrue: => F[A], ifFalse: => F[A]): F[A] =
				cond >>= (if _ then ifTrue else ifFalse)
			def and(l: F[Boolean], r: F[Boolean]): F[Boolean] = Apply[F].map2(l, r)(_ && _)
			def or(l: F[Boolean], r: F[Boolean]): F[Boolean] = Apply[F].map2(l, r)(_ || _)
			def not(a: F[Boolean]): F[Boolean] = a.map(!_)
			def equals[A: Type](l: F[A], r: F[A]): F[Boolean] = Apply[F].map2(l, r)(_ == _)
			def notEquals[A: Type](l: F[A], r: F[A]): F[Boolean] = Apply[F].map2(l, r)(_ != _)
			def < [A: {Type, Ordering}](l: F[A], r: F[A]): F[Boolean] = Apply[F].map2(l, r)(Ordering[A].lt)
			def <=[A: {Type, Ordering}](l: F[A], r: F[A]): F[Boolean] = Apply[F].map2(l, r)(Ordering[A].lteq)
			def > [A: {Type, Ordering}](l: F[A], r: F[A]): F[Boolean] = Apply[F].map2(l, r)(Ordering[A].gt)
			def >=[A: {Type, Ordering}](l: F[A], r: F[A]): F[Boolean] = Apply[F].map2(l, r)(Ordering[A].gteq)
			def leftEntuple[A, T <: NonEmptyTuple](a: F[A], t: F[T]): F[A *: T] = Apply[F].map2(a, t)(_ *: _)
			def rightEntuple[T <: NonEmptyTuple, A](t: F[T], a: F[A]): F[Tuple.Append[T, A]] = Apply[F].map2(t, a)(_ :* _)
			def *>[A, B](l: F[A], r: F[B]): F[B] = Apply[F].productR(l)(r)

	given Ops[[a] =>> String] = new Ops[[a] =>> String]:
		def value[A: Type](v: A): String = v.toString
		def add[N: {Type, Numeric}](l: String, r: String): String = s"($l + $r)"
		def sub[N: {Type, Numeric}](l: String, r: String): String = s"($l - $r)"
		def mul[N: {Type, Numeric}](l: String, r: String): String = s"($l * $r)"
		def div[N: {Type, Fractional}](l: String, r: String): String = s"($l / $r)"
		def quot[N: {Type, Integral}](l: String, r: String): String = s"($l / $r)"
		def mod[N: {Type, Integral}](l: String, r: String): String = s"($l % $r)"
		def neg[N: {Type, Numeric}](a: String): String = s"(-$a)"
		def abs[N: {Type, Numeric}](a: String): String = s"abs($a)"
		def len(fa: String): String = s"$fa.len"
		def toDouble(fa: String): String = s"$fa.toDouble"
		def concat(l: String, r: String): String = s"($l ++ $r)"
		def repeatUntil[A](action: => String)(condition: => String): String = s"repeat { $action } until { $condition }"
		def whileDo[A](condition: => String)(action: => String): String = s"while { $condition } do { $action }"
		def ifElse[A](cond: String)(ifTrue: => String, ifFalse: => String): String = s"if ($cond) { $ifTrue } else { $ifFalse }"
		def and(l: String, r: String): String = s"($l && $r)"
		def or(l: String, r: String): String = s"($l || $r)"
		def not(a: String): String = s"(!$a)"
		def equals[A: Type](l: String, r: String): String = s"($l == $r)"
		def notEquals[A: Type](l: String, r: String): String = s"($l != $r)"
		def < [A: {Type, Ordering}](l: String, r: String): String = s"($l < $r)"
		def <=[A: {Type, Ordering}](l: String, r: String): String = s"($l <= $r)"
		def > [A: {Type, Ordering}](l: String, r: String): String = s"($l > $r)"
		def >=[A: {Type, Ordering}](l: String, r: String): String = s"($l >= $r)"
		def leftEntuple[A, T <: NonEmptyTuple](a: String, t: String): String = s"($a, $t)"
		def rightEntuple[T <: NonEmptyTuple, A](t: String, a: String): String = s"($t, $a)"
		def *>[A, B](l: String, r: String): String = s"$l\n$r"

	given Ops[Type] = new Ops[Type]:
		def value[A: Type](v: A): Type[A] = Type[A]
		def add[N: {Type, Numeric}](l: Type[N], r: Type[N]): Type[N] = Type[N]
		def sub[N: {Type, Numeric}](l: Type[N], r: Type[N]): Type[N] = Type[N]
		def mul[N: {Type, Numeric}](l: Type[N], r: Type[N]): Type[N] = Type[N]
		def div[N: {Type, Fractional}](l: Type[N], r: Type[N]): Type[N] = Type[N]
		def quot[N: {Type, Integral}](l: Type[N], r: Type[N]): Type[N] = Type[N]
		def mod[N: {Type, Integral}](l: Type[N], r: Type[N]): Type[N] = Type[N]
		def neg[N: {Type, Numeric}](a: Type[N]): Type[N] = Type[N]
		def abs[N: {Type, Numeric}](a: Type[N]): Type[N] = Type[N]
		def len(fa: Type[String]): Type[Int] = Type[Int]
		def toDouble(fa: Type[Int]): Type[Double] = Type[Double]
		def concat(l: Type[String], r: Type[String]): Type[String] = Type[String]
		def repeatUntil[A](action: => Type[A])(condition: => Type[Boolean]): Type[A] = action
		def whileDo[A](condition: => Type[Boolean])(action: => Type[A]): Type[Unit] = Type[Unit]
		def ifElse[A](cond: Type[Boolean])(ifTrue: => Type[A], ifFalse: => Type[A]): Type[A] = ifTrue
		def and(l: Type[Boolean], r: Type[Boolean]): Type[Boolean] = Type[Boolean]
		def or(l: Type[Boolean], r: Type[Boolean]): Type[Boolean] = Type[Boolean]
		def not(a: Type[Boolean]): Type[Boolean] = Type[Boolean]
		def equals[A: Type](l: Type[A], r: Type[A]): Type[Boolean] = Type[Boolean]
		def notEquals[A: Type](l: Type[A], r: Type[A]): Type[Boolean] = Type[Boolean]
		def < [A: {Type, Ordering}](l: Type[A], r: Type[A]): Type[Boolean] = Type[Boolean]
		def <=[A: {Type, Ordering}](l: Type[A], r: Type[A]): Type[Boolean] = Type[Boolean]
		def > [A: {Type, Ordering}](l: Type[A], r: Type[A]): Type[Boolean] = Type[Boolean]
		def >=[A: {Type, Ordering}](l: Type[A], r: Type[A]): Type[Boolean] = Type[Boolean]
		def leftEntuple[A, T <: NonEmptyTuple](a: Type[A], t: Type[T]): Type[A *: T] =
			Type.TupLeftType[A, T](using a, t)
		def rightEntuple[T <: NonEmptyTuple, A](t: Type[T], a: Type[A]): Type[Tuple.Append[T, A]] =
			Type.TupRightType[T, A](using t, a)
		def *>[A, B](l: Type[A], r: Type[B]): Type[B] = r

	type IdentState[A] = State[Int, String]
	extension (s: State[Int, String])
		def ident: State[Int, String] = s.flatMap(str => State.get.map(id => (" " * id) + str))
	extension(s: String)
		def ident(n: Int): String = (" " * n) + s

	given Ops[IdentState] = new Ops[IdentState]:
		def value[A: Type](v: A): IdentState[String] = State.pure(v.toString).ident
		def add[N: {Type, Numeric}](l: IdentState[N], r: IdentState[N]): IdentState[N] =
			l.flatMap(l => r.map(r => s"($l + $r)"))
		def sub[N: {Type, Numeric}](l: IdentState[N], r: IdentState[N]): IdentState[N] =
			l.flatMap(l => r.map(r => s"($l - $r)"))
		def mul[N: {Type, Numeric}](l: IdentState[N], r: IdentState[N]): IdentState[N] =
			l.flatMap(l => r.map(r => s"($l * $r)"))
		def div[N: {Type, Fractional}](l: IdentState[N], r: IdentState[N]): IdentState[N] =
			l.flatMap(l => r.map(r => s"($l / $r)"))
		def quot[N: {Type, Integral}](l: IdentState[N], r: IdentState[N]): IdentState[N] =
			l.flatMap(l => r.map(r => s"($l / $r)"))
		def mod[N: {Type, Integral}](l: IdentState[N], r: IdentState[N]): IdentState[N] =
			l.flatMap(l => r.map(r => s"($l % $r)"))
		def neg[N: {Type, Numeric}](a: IdentState[N]): IdentState[N] =
			a.map(a => s"(-$a)")
		def abs[N: {Type, Numeric}](a: IdentState[N]): IdentState[N] =
			a.map(a => s"abs($a)")
		def len(fa: IdentState[String]): IdentState[String] =
			fa.map(v => s"$v.len")
		def toDouble(fa: IdentState[Int]): IdentState[Double] =
			fa.map(v => s"$v.toDouble")
		def concat(l: IdentState[String], r: IdentState[String]): IdentState[String] =
			l.flatMap(l => r.map(r => s"($l ++ $r)"))
		def repeatUntil[A](action: => IdentState[String])(condition: => IdentState[String]): IdentState[String] =
			State { i =>
				val act = action.runA(i + 2).value
				val cond = condition.runA(i + 2).value
				i -> ("repeat {" + "\n" + act.ident(i + 2) + "} until {".ident(i + 2) + "\n" + cond.ident(i + 2) + "}".ident(i))
			}
		def whileDo[A](condition: => IdentState[String])(action: => IdentState[String]): IdentState[String] =
			State { i =>
				val act = action.runA(i + 2).value
				val cond = condition.runA(i + 2).value
				i -> ("do {" + "\n" + act.ident(i + 2) + "} while {".ident(i) + "\n" + cond.ident(i + 2) + "}".ident(i))
			}
		def ifElse[A](cond: IdentState[String])(ifTrue: => IdentState[String], ifFalse: => IdentState[String]): IdentState[String] =
			State { i =>
				val cnd = cond.runA(i).value
				val ift = ifTrue.runA(i + 2).value
				val iff = ifFalse.runA(i + 2).value
				i -> (s"if ($cnd) {" + "\n" + ift.ident(i + 2) + "} else {".ident(i) + "\n" + iff.ident(i + 2) + "\n" + "}".ident(i))
			}
		def and(l: IdentState[String], r: IdentState[String]): IdentState[String] =
			l.flatMap(l => r.map(r => s"($l && $r)"))
		def or(l: IdentState[String], r: IdentState[String]): IdentState[String] =
			l.flatMap(l => r.map(r => s"($l || $r)"))
		def not(a: IdentState[String]): IdentState[String] =
			a.map(a => s"(!$a)")
		def equals[A: Type](l: IdentState[String], r: IdentState[String]): IdentState[String] =
			l.flatMap(l => r.map(r => s"($l == $r)"))
		def notEquals[A: Type](l: IdentState[String], r: IdentState[String]): IdentState[String] =
			l.flatMap(l => r.map(r => s"($l != $r)"))
		def <[A: {Type, Ordering}](l: IdentState[String], r: IdentState[String]): IdentState[String] =
			l.flatMap(l => r.map(r => s"($l < $r)"))
		def <=[A: {Type, Ordering}](l: IdentState[String], r: IdentState[String]): IdentState[String] =
			l.flatMap(l => r.map(r => s"($l <= $r)"))
		def >[A: {Type, Ordering}](l: IdentState[String], r: IdentState[String]): IdentState[String] =
			l.flatMap(l => r.map(r => s"($l > $r)"))
		def >=[A: {Type, Ordering}](l: IdentState[String], r: IdentState[String]): IdentState[String] =
			l.flatMap(l => r.map(r => s"($l >= $r)"))
		def leftEntuple[A, T <: NonEmptyTuple](a: IdentState[String], t: IdentState[String]): IdentState[String] =
			State(id => id -> s"(${a.runA(id).value}, ${t.runA(id).value})")
		def rightEntuple[T <: NonEmptyTuple, A](t: IdentState[String], a: IdentState[String]): IdentState[String] =
			State(id => id -> s"(${t.runA(id).value}, ${a.runA(id).value})")
		def *>[A, B](l: IdentState[String], r: IdentState[String]): IdentState[String] =
			State(id => id -> (l.runA(id).value + "\n" + r.runA(id).value.ident(id)))
		

end Ops
