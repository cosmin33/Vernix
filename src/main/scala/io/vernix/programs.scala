package io.vernix

import cats.Eval
import cats.data.State

import scala.util.Try

trait Program[A]:
	self =>
	def apply[F[_]: Statements]: F[A]
	def +(that: Program[A])(using Type[A], Numeric[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].add(self[F], that[F])
	def -(that: Program[A])(using Type[A], Numeric[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].sub(self[F], that[F])
	def *(that: Program[A])(using Type[A], Numeric[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].mul(self[F], that[F])
	def /(that: Program[A])(using Type[A], Fractional[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].div(self[F], that[F])
	def quot(that: Program[A])(using Type[A], Integral[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].quot(self[F], that[F])
	def %(that: Program[A])(using Type[A], Integral[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].mod(self[F], that[F])
	def neg(using Type[A], Numeric[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].neg(self[F])
	def abs(using Type[A], Numeric[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].abs(self[F])
	def ++(that: Program[String])(using e: A =:= String): Program[String] = new Program[String]:
		def apply[F[_]: Statements]: F[String] = Statements[F].concat(e.substituteCo(self[F]), that[F])
	def toDouble(using e: A =:= Int): Program[Double] = new Program[Double]:
		def apply[F[_]: Statements]: F[Double] = Statements[F].toDouble(e.substituteCo(self[F]))
	def nest: Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].nest(self[F])
	def repeatUntil(condition: Program[Boolean]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].repeatUntil(self[F])(condition[F])
	def leftEntuple[T <: NonEmptyTuple](t: Program[T]): Program[A *: T] = new Program[A *: T]:
		def apply[F[_]: Statements]: F[A *: T] = Statements[F].leftEntuple(self[F], t[F])
	def rightEntuple[T <: NonEmptyTuple](t: Program[T]): Program[Tuple.Append[T, A]] = new Program[Tuple.Append[T, A]]:
		def apply[F[_]: Statements]: F[Tuple.Append[T, A]] = Statements[F].rightEntuple(t[F], self[F])
	def *>[B](that: Program[B]): Program[B] = new Program[B]:
		def apply[F[_]: Statements]: F[B] = Statements[F].*>(self[F], that[F])
	def &&(that: Program[Boolean])(using e: A =:= Boolean): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: Statements]: F[Boolean] = Statements[F].and(e.substituteCo(self[F]), that[F])
	def ||(that: Program[Boolean])(using e: A =:= Boolean): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: Statements]: F[Boolean] = Statements[F].or(e.substituteCo(self[F]), that[F])
	def unary_!(using e: A =:= Boolean): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: Statements]: F[Boolean] = Statements[F].not(e.substituteCo(self[F]))
	def ===(that: Program[A])(using Type[A]): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: Statements]: F[Boolean] = Statements[F].equals(self[F], that[F])
	def !==(that: Program[A])(using Type[A]): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: Statements]: F[Boolean] = Statements[F].notEquals(self[F], that[F])
	def <(that: Program[A])(using Type[A], Ordering[A]): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: Statements]: F[Boolean] = Statements[F].`<`(self[F], that[F])
	def <=(that: Program[A])(using Type[A], Ordering[A]): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: Statements]: F[Boolean] = Statements[F].`<=`(self[F], that[F])
	def >(that: Program[A])(using Type[A], Ordering[A]): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: Statements]: F[Boolean] = Statements[F].`>`(self[F], that[F])
	def >=(that: Program[A])(using Type[A], Ordering[A]): Program[Boolean] = new Program[Boolean]:
		def apply[F[_]: Statements]: F[Boolean] = Statements[F].`>=`(self[F], that[F])
	
	def compilation(c: OpContext = OpContext.empty): Eval[Expr[A]] = self[[a] =>> State[OpContext, Expr[a]]].runA(c)
	def compile: Try[Expr[A]] = Try(compilation().value)
object Program:
	def value[A: Type](v: A): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].value(v)
	def variable[A: Type](name: String): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].variable[A](name)
	def let[A: Type](name: String, value: Program[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].addVar(name, value[F])
	def funDef[A: Type, B: Type](name: String, param: String, body: Program[B]): Program[Unit] = new Program[Unit]:
		def apply[F[_]: Statements]: F[Unit] = Statements[F].funDef[A, B](name, param, body[F])
	def funCall[A: Type, B: Type](name: String, param: Program[A]): Program[B] = new Program[B]:
		def apply[F[_]: Statements]: F[B] = Statements[F].funCall(name, param[F])
	def repeatUntil[A](action: Program[A])(condition: Program[Boolean]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].repeatUntil(action[F])(condition[F])
	def whileDo[A](condition: Program[Boolean])(action: Program[A]): Program[Unit] = new Program[Unit]:
		def apply[F[_]: Statements]: F[Unit] = Statements[F].whileDo(condition[F])(action[F])
	def ifElse[A](cond: Program[Boolean])(ifTrue: Program[A], ifFalse: Program[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].ifElse(cond[F])(ifTrue[F], ifFalse[F])
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
