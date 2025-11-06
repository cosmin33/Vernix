package io.vernix

import cats.Eval
import cats.data.State

import scala.util.Try

trait Program[A]:
	self =>
	def apply[F[_]: {Ops, Statements}]: F[A]

	def len(using e: A =:= String): Program[Int] = new Program[Int]:
		def apply[F[_]: {Ops, Statements}]: F[Int] = Ops[F].len(e.substituteCo(self[F]))
	def toDouble(using e: A =:= Int): Program[Double] = new Program[Double]:
		def apply[F[_]: {Ops, Statements}]: F[Double] = Ops[F].toDouble(e.substituteCo(self[F]))
	def +(that: Program[A])(using Type[A], Numeric[A]): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Ops[F].add(self[F], that[F])
	def -(that: Program[A])(using Type[A], Numeric[A]): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Ops[F].sub(self[F], that[F])
	def *(that: Program[A])(using Type[A], Numeric[A]): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Ops[F].mul(self[F], that[F])
	def /(that: Program[A])(using Type[A], Fractional[A]): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Ops[F].div(self[F], that[F])
	def quot(that: Program[A])(using Type[A], Integral[A]): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Ops[F].quot(self[F], that[F])
	def %(that: Program[A])(using Type[A], Integral[A]): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Ops[F].mod(self[F], that[F])
	def neg(using Type[A], Numeric[A]): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Ops[F].neg(self[F])
	def abs(using Type[A], Numeric[A]): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Ops[F].abs(self[F])
	def ++(that: Program[String])(using e: A =:= String): Program[String] = new Program[String]:
		def apply[F[_]: {Ops, Statements}]: F[String] = Ops[F].concat(e.substituteCo(self[F]), that[F])
	def repeatUntil(condition: Program[Boolean]): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Ops[F].repeatUntil(self[F])(condition[F])
	def ifElse[B](ifTrue: Program[B], ifFalse: Program[B])(using e: A =:= Boolean): Program[B] = new Program[B]:
		def apply[F[_]: {Ops, Statements}]: F[B] = Ops[F].ifElse(e.substituteCo(self[F]))(ifTrue[F], ifFalse[F])
	def &&(that: Program[Boolean])(using e: A =:= Boolean): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: {Ops, Statements}]: F[Boolean] = Ops[F].and(e.substituteCo(self[F]), that[F])
	def ||(that: Program[Boolean])(using e: A =:= Boolean): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: {Ops, Statements}]: F[Boolean] = Ops[F].or(e.substituteCo(self[F]), that[F])
	def unary_!(using e: A =:= Boolean): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: {Ops, Statements}]: F[Boolean] = Ops[F].not(e.substituteCo(self[F]))
	def ===(that: Program[A])(using Type[A]): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: {Ops, Statements}]: F[Boolean] = Ops[F].equals(self[F], that[F])
	def !==(that: Program[A])(using Type[A]): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: {Ops, Statements}]: F[Boolean] = Ops[F].notEquals(self[F], that[F])
	def <(that: Program[A])(using Type[A], Ordering[A]): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: {Ops, Statements}]: F[Boolean] = Ops[F].`<`(self[F], that[F])
	def <=(that: Program[A])(using Type[A], Ordering[A]): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: {Ops, Statements}]: F[Boolean] = Ops[F].`<=`(self[F], that[F])
	def >(that: Program[A])(using Type[A], Ordering[A]): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: {Ops, Statements}]: F[Boolean] = Ops[F].`>`(self[F], that[F])
	def >=(that: Program[A])(using Type[A], Ordering[A]): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: {Ops, Statements}]: F[Boolean] = Ops[F].`>=`(self[F], that[F])
	def leftEntuple[T <: NonEmptyTuple](t: Program[T]): Program[A *: T] = new Program[A *: T]:
		def apply[F[_]: {Ops, Statements}]: F[A *: T] = Ops[F].leftEntuple(self[F], t[F])
	def rightEntuple[T <: NonEmptyTuple](t: Program[T]): Program[Tuple.Append[T, A]] = new Program[Tuple.Append[T, A]]:
		def apply[F[_]: {Ops, Statements}]: F[Tuple.Append[T, A]] = Ops[F].rightEntuple(t[F], self[F])
	def *>[B](that: Program[B]): Program[B] = new Program[B]:
		def apply[F[_]: {Ops, Statements}]: F[B] = Ops[F].*>(self[F], that[F])
object Program:
	def value[A: Type](v: A): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Ops[F].value(v)
	// VarOps specific
	def variable[A: Type](name: String): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Statements[F].variable[A](name)
	def addVar[A: Type](name: String, value: Program[A]): Program[Unit] = new Program[Unit]:
		def apply[F[_]: {Ops, Statements}]: F[Unit] = Statements[F].addVar[A](name, value[F])
	def setVar[A: Type](name: String, value: Program[A]): Program[Unit] = new Program[Unit]:
		def apply[F[_]: {Ops, Statements}]: F[Unit] = Statements[F].setVar[A](name, value[F])
	def nest[A: Type](fa: Program[A]): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Statements[F].nest(fa[F])
	def whileDo[A](condition: Program[Boolean])(action: Program[A]): Program[Unit] = new Program[Unit]:
		def apply[F[_]: {Ops, Statements}]: F[Unit] = Ops[F].whileDo(condition[F])(action[F])
	def repeatUntil[A](action: Program[A])(condition: Program[Boolean]): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Ops[F].repeatUntil(action[F])(condition[F])
	def ifElse[A](cond: Program[Boolean])(ifTrue: Program[A], ifFalse: Program[A]): Program[A] = new Program[A]:
		def apply[F[_]: {Ops, Statements}]: F[A] = Ops[F].ifElse(cond[F])(ifTrue[F], ifFalse[F])
end Program

trait Prog:
	type T
	def `type`: Type[T]
	def program: Program[T]
	def get[A](using eq: T =:= A): Program[A] = eq.substituteCo(program)
	def unsafe[A: Type]: Program[A] =
		if (Type[A].name == `type`.name) program.asInstanceOf[Program[A]]
		else throw new ClassCastException(s"Program is of type ${`type`.name}, not ${Type[A].name}")
object Prog:
	type Aux[A] = Prog { type T = A }
	def apply[A: Type](p: Program[A]): Prog.Aux[A] = new Prog:
		type T = A
		val `type`: Type[A] = summon
		val program: Program[A] = p
	extension[T] (p: Program[T])
		def prog(using Type[T]): Prog.Aux[T] = Prog(p)

end Prog
