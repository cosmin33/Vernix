package io.vernix

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
	def neg(using Type[A], Numeric[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].neg(self[F])
	def abs(using Type[A], Numeric[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].abs(self[F])
	def ++(that: Program[String])(using e: A =:= String): Program[String] = new Program[String]:
		def apply[F[_]: Statements]: F[String] = Statements[F].concat(e.substituteCo(self[F]), that[F])
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
	def compile: Try[Expr[A]] =
		Try(self[[a] =>> State[OpContext, Expr[a]]](using Statements.stateStatements).runA(OpContext.empty).value)
object Program:
	def value[A: Type](v: A): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].value(v)
	def variable[A: Type](name: String): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].variable[A](name)
	def let[A: Type](name: String, value: Program[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].let(name, value[F])
	def funDef[A: Type, B: Type](name: String, param: String, body: Program[B]): Program[Unit] = new Program[Unit]:
		def apply[F[_]: Statements]: F[Unit] = Statements[F].funDef[A, B](name, param, body[F])
	def funCall[A: Type, B: Type](name: String, param: Program[A]): Program[B] = new Program[B]:
		def apply[F[_]: Statements]: F[B] = Statements[F].funCall(name, param[F])
	def doWhile[A](condition: Program[Boolean])(action: Program[A]): Program[Unit] = new Program[Unit]:
		def apply[F[_]: Statements]: F[Unit] = Statements[F].doWhile(condition[F])(action[F])
	def ifElse[A](cond: Program[Boolean])(ifTrue: Program[A], ifFalse: Program[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].ifElse(cond[F])(ifTrue[F], ifFalse[F])
end Program
