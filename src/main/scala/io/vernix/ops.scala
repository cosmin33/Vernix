package io.vernix

import scala.util.Try
import zio.*
import cats.data.*
import cats.syntax.flatMap.*

trait Ops[F[_]]:
	def typeK: TypeK[F]
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
	def repeatUntil[A](action: F[A])(condition: F[Boolean]): F[A]
	def whileDo[A](condition: F[Boolean])(action: F[A]): F[Unit]
	def ifElse[A](cond: F[Boolean])(ifTrue: F[A], ifFalse: F[A]): F[A]
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
	//def size[L[x] <: IterableOnce[x], A](fa:F[L[A]]): F[Int]
	//def merge[A, B, C](fa: F[A])(left: F[MappingFunc[A, C]], right: F[MappingFunc[B, C]]): F[(B, C)]
	def *>[A, B](l: F[A], r: F[B]): F[B]
object Ops:
	def apply[F[_]](using ops: Ops[F]): Ops[F] = ops

	given Ops[Task] =
		new Ops[Task]:
			val typeK: TypeK[Task] = new TypeK[Task]:
				def name: String = "Task"
			def value[A: Type](v: A): Task[A] = ZIO.succeed(v)
			def add[N: {Type, Numeric}](l: Task[N], r: Task[N]): Task[N] = l.zipWith(r)(Numeric[N].plus)
			def mul[N: {Type, Numeric}](l: Task[N], r: Task[N]): Task[N] = l.zipWith(r)(Numeric[N].times)
			def sub[N: {Type, Numeric}](l: Task[N], r: Task[N]): Task[N] = l.zipWith(r)(Numeric[N].minus)
			def div[N: {Type, Fractional}](l: Task[N], r: Task[N]): Task[N] = l.zipWith(r)(Fractional[N].div)
			def quot[N: {Type, Integral}](l: Task[N], r: Task[N]): Task[N] = l.zipWith(r)(Integral[N].quot)
			def mod[N: {Type, Integral}](l: Task[N], r: Task[N]): Task[N] = l.zipWith(r)(Integral[N].rem)
			def neg[N: {Type, Numeric}](a: Task[N]): Task[N] = a.map(Numeric[N].negate)
			def abs[N: {Type, Numeric}](a: Task[N]): Task[N] = a.map(Numeric[N].abs)
			def len(fa: Task[String]): Task[Int] = fa.map(_.length)
			def toDouble(fa: Task[Int]): Task[Double] = fa.map(_.toDouble)
			def concat(l: Task[String], r: Task[String]): Task[String] = l.zipWith(r)(_ + _)
			def repeatUntil[A](action: Task[A])(condition: Task[Boolean]): Task[A] =
				action.flatMap(a => condition.flatMap(
					if _ then ZIO.succeed(a)
					else repeatUntil(action)(condition)
				))
			def whileDo[A](condition: Task[Boolean])(action: Task[A]): Task[Unit] =
				condition.flatMap(ZIO.unlessDiscard(_)(action *> whileDo(condition)(action)))
			def ifElse[A](cond: Task[Boolean])(ifTrue: Task[A], ifFalse: Task[A]): Task[A] =
				cond.flatMap(if _ then ifTrue else ifFalse)
			def and(l: Task[Boolean], r: Task[Boolean]): Task[Boolean] = l.zipWith(r)(_ && _)
			def or(l: Task[Boolean], r: Task[Boolean]): Task[Boolean] = l.zipWith(r)(_ || _)
			def not(a: Task[Boolean]): Task[Boolean] = a.map(!_)
			def equals[A: Type](l: Task[A], r: Task[A]): Task[Boolean] = l.zipWith(r)(_ == _)
			def notEquals[A: Type](l: Task[A], r: Task[A]): Task[Boolean] = l.zipWith(r)(_ != _)
			def < [A: {Type, Ordering}](l: Task[A], r: Task[A]): Task[Boolean] = l.zipWith(r)(Ordering[A].lt)
			def <=[A: {Type, Ordering}](l: Task[A], r: Task[A]): Task[Boolean] = l.zipWith(r)(Ordering[A].lteq)
			def > [A: {Type, Ordering}](l: Task[A], r: Task[A]): Task[Boolean] = l.zipWith(r)(Ordering[A].gt)
			def >=[A: {Type, Ordering}](l: Task[A], r: Task[A]): Task[Boolean] = l.zipWith(r)(Ordering[A].gteq)
			def leftEntuple[A, T <: NonEmptyTuple](a: Task[A], t: Task[T]): Task[A *: T] = a.zipWith(t)(_ *: _)
			def rightEntuple[T <: NonEmptyTuple, A](t: Task[T], a: Task[A]): Task[Tuple.Append[T, A]] = t.zipWith(a)(_ :* _)
			def *>[A, B](l: Task[A], r: Task[B]): Task[B] = l *> r

	given Ops[Try] =
		new Ops[Try]:
			val typeK: TypeK[Try] = new TypeK[Try]:
				def name: String = "Try"
			def value[A: Type](v: A): Try[A] = scala.util.Try(v)
			def add[N: {Type, Numeric}](l: Try[N], r: Try[N]): Try[N] = l.flatMap(a => r.map(b => Numeric[N].plus(a, b)))
			def sub[N: {Type, Numeric}](l: Try[N], r: Try[N]): Try[N] = l.flatMap(a => r.map(b => Numeric[N].minus(a, b)))
			def mul[N: {Type, Numeric}](l: Try[N], r: Try[N]): Try[N] = l.flatMap(a => r.map(b => Numeric[N].times(a, b)))
			def div[N: {Type, Fractional}](l: Try[N], r: Try[N]): Try[N] = l.flatMap(a => r.map(b => Fractional[N].div(a, b)))
			def quot[N: {Type, Integral}](l: Try[N], r: Try[N]): Try[N] = l.flatMap(a => r.map(b => Integral[N].quot(a, b)))
			def mod[N: {Type, Integral}](l: Try[N], r: Try[N]): Try[N] = l.flatMap(a => r.map(b => Integral[N].rem(a, b)))
			def neg[N: {Type, Numeric}](a: Try[N]): Try[N] = a.map(Numeric[N].negate)
			def abs[N: {Type, Numeric}](a: Try[N]): Try[N] = a.map(Numeric[N].abs)
			def len(fa: Try[String]): Try[Int] = fa.map(_.length)
			def toDouble(fa: Try[Int]): Try[Double] = fa.map(_.toDouble)
			def concat(l: Try[String], r: Try[String]): Try[String] = l.flatMap(a => r.map(b => a + b))
			def repeatUntil[A](action: Try[A])(condition: Try[Boolean]): Try[A] =
				action >>= (a => condition >>= (if _ then Try(a) else repeatUntil(action)(condition)))
			def whileDo[A](condition: Try[Boolean])(action: Try[A]): Try[Unit] =
				condition.flatMap(if _ then Try(()) else action.flatMap(_ => whileDo(condition)(action)))
			def ifElse[A](cond: Try[Boolean])(ifTrue: Try[A], ifFalse: Try[A]): Try[A] =
				cond.flatMap(if _ then ifTrue else ifFalse)
			def and(l: Try[Boolean], r: Try[Boolean]): Try[Boolean] = l.flatMap(a => r.map(b => a && b))
			def or(l: Try[Boolean], r: Try[Boolean]): Try[Boolean] = l.flatMap(a => r.map(b => a || b))
			def not(a: Try[Boolean]): Try[Boolean] = a.map(!_)
			def equals[A: Type](l: Try[A], r: Try[A]): Try[Boolean] = l.flatMap(a => r.map(b => a == b))
			def notEquals[A: Type](l: Try[A], r: Try[A]): Try[Boolean] = l.flatMap(a => r.map(b => a != b))
			def < [A: {Type, Ordering}](l: Try[A], r: Try[A]): Try[Boolean] = l.flatMap(a => r.map(b => Ordering[A].lt(a, b)))
			def <=[A: {Type, Ordering}](l: Try[A], r: Try[A]): Try[Boolean] = l.flatMap(a => r.map(b => Ordering[A].lteq(a, b)))
			def > [A: {Type, Ordering}](l: Try[A], r: Try[A]): Try[Boolean] = l.flatMap(a => r.map(b => Ordering[A].gt(a, b)))
			def >=[A: {Type, Ordering}](l: Try[A], r: Try[A]): Try[Boolean] = l.flatMap(a => r.map(b => Ordering[A].gteq(a, b)))
			def leftEntuple[A, T <: NonEmptyTuple](a: Try[A], t: Try[T]): Try[A *: T] = a.flatMap(a => t.map(t => a *: t))
			def rightEntuple[T <: NonEmptyTuple, A](t: Try[T], a: Try[A]): Try[Tuple.Append[T, A]] =
				t.flatMap(t => a.map(a => t :* a))
			def *>[A, B](l: Try[A], r: Try[B]): Try[B] = l.flatMap(_ => r)

	given Statements[[a] =>> String] = new Statements[[a] =>> String]:
		val typeK: TypeK[[a] =>> String] = new TypeK[[a] =>> String]:
			def name: String = "[a] =>> String"
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
		def repeatUntil[A](action: String)(condition: String): String = s"repeat { $action } until { $condition }"
		def whileDo[A](condition: String)(action: String): String = s"do { $action } while { $condition }"
		def ifElse[A](cond: String)(ifTrue: String, ifFalse: String): String = s"if ($cond) { $ifTrue } else { $ifFalse }"
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
		def variable[A: Type](name: String): String = s"$name"
		def addVar[A: Type](name: String, value: String): String = s"var $name: ${Type[A].name} = $value"
		def setVar[A: Type](name: String, value: String): String = s"$name = $value"
		def nest[A](fa: String): String = s"{$fa}"
		def funDef[A: Type, B: Type](name: String, param: String, body: String): String =
			s"def $name($param: ${Type[A].name}): ${Type[B].name} = $body"
		def funCall[A: Type, B: Type](name: String, param: String): String =
			s"$name($param)"

	type Id[A] = A

	given Statements[Type] = new Statements[Type]:
		val typeK: TypeK[Type] = new TypeK[Type]:
			def name: String = "Type"
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
		def repeatUntil[A](action: Type[A])(condition: Type[Boolean]): Type[A] = action
		def whileDo[A](condition: Type[Boolean])(action: Type[A]): Type[Unit] = Type[Unit]
		def ifElse[A](cond: Type[Boolean])(ifTrue: Type[A], ifFalse: Type[A]): Type[A] = ifTrue
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
		// Statements[Type] specific
		def variable[A: Type](name: String): Type[A] = Type[A]
		def addVar[A: Type](name: String, value: Type[A]): Type[A] = value
		def setVar[A: Type](name: String, value: Type[A]): Type[A] = value
		def nest[A](fa: Type[A]): Type[A] = fa
		def funDef[A: Type, B: Type](name: String, param: String, body: Type[B]): Type[Unit] = Type[Unit]
		def funCall[A: Type, B: Type](name: String, param: Type[A]): 	Type[B] = Type[B]

	type IdentState[A] = State[Int, String]
	extension (s: State[Int, String])
		def ident: State[Int, String] = s.flatMap(str => State.get.map(id => (" " * id) + str))
	extension(s: String)
		def ident(n: Int): String = (" " * n) + s

	given Statements[IdentState] = new Statements[IdentState]:
		val typeK: TypeK[IdentState] = new TypeK[IdentState]:
			def name: String = "IdentState"
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
		def repeatUntil[A](action: IdentState[String])(condition: IdentState[String]): IdentState[String] =
			State { i =>
				val act = action.runA(i + 2).value
				val cond = condition.runA(i + 2).value
				i -> ("repeat {" + "\n" + act.ident(i + 2) + "} until {".ident(i + 2) + "\n" + cond.ident(i + 2) + "}".ident(i))
			}
		def whileDo[A](condition: IdentState[String])(action: IdentState[String]): IdentState[String] =
			State { i =>
				val act = action.runA(i + 2).value
				val cond = condition.runA(i + 2).value
				i -> ("do {" + "\n" + act.ident(i + 2) + "} while {".ident(i) + "\n" + cond.ident(i + 2) + "}".ident(i))
			}
		def ifElse[A](cond: IdentState[String])(ifTrue: IdentState[String], ifFalse: IdentState[String]): IdentState[String] =
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
		// Statements[IdentState] specific
		def variable[A: Type](name: String): IdentState[A] = State.pure(s"$name")
		def addVar[A: Type](name: String, value: IdentState[A]): IdentState[A] =
			value.map(v => s"var $name: ${Type[A].name} = $v")
		def setVar[A: Type](name: String, value: IdentState[A]): IdentState[A] =
			value.map(v => s"$name = $v")
		def nest[A](fa: IdentState[A]): IdentState[A] =
			State(i => i -> ("{" + "\n" + fa.runA(i + 2).value.ident(i) + "\n" + "}".ident(i)))
		def funDef[A: Type, B: Type](name: String, param: String, body: IdentState[B]): IdentState[Unit] =
			State { i =>
				val b = body.runA(i + 2).value
				i -> (s"def $name($param: ${Type[A].name}): ${Type[B].name} {" + "\n" + b.ident(i + 2) + "\n" + "}".ident(i))
			}
		def funCall[A: Type, B: Type](name: String, param: IdentState[A]): IdentState[B] =
			param.map(p => s"$name($p)")


end Ops
