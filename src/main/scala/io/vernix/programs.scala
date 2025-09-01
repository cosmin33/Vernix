package io.vernix

import cats.data.State

import scala.util.Try

trait Program[A]:
	self =>
	def apply[F[_]: Statements]: F[A]
	def +(that: Program[Int])(using e: A =:= Int): Program[Int] = new Program[Int]:
		def apply[F[_]: Statements]: F[Int] = Statements[F].add(e.substituteCo(self[F]), that[F])
	def *(that: Program[Int])(using e: A =:= Int): Program[Int] = new Program[Int]:
		def apply[F[_]: Statements]: F[Int] = Statements[F].mul(e.substituteCo(self[F]), that[F])
	def ++(that: Program[String])(using e: A =:= String): Program[String] = new Program[String]:
		def apply[F[_]: Statements]: F[String] = Statements[F].concat(e.substituteCo(self[F]), that[F])
	def nest: Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].nest(self[F])
	def *>[B](that: Program[B]): Program[B] = new Program[B]:
		def apply[F[_]: Statements]: F[B] = Statements[F].*>(self[F], that[F])
object Program:
	def value[A](v: A): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].value(v)
	def variable[A: Type](name: String): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].variable[A](name)
	def let[A: Type](name: String, value: Program[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].let(name, value[F])
	def funDef[A: Type, B: Type](name: String, param: String, body: Program[B]): Program[Unit] = new Program[Unit]:
		def apply[F[_]: Statements]: F[Unit] = Statements[F].funDef[A, B](name, param, body[F])
	def function[A: Type, B: Type](name: String, param: Program[A]): Program[B] = new Program[B]:
		def apply[F[_]: Statements]: F[B] = Statements[F].function(name, param[F])
		
	def compile[A](ctx: OpContext): Program[A] => Try[Expr[A]] =
		pe => Try(pe[[a] =>> State[OpContext, Expr[a]]](using Statements.stateStatements).runA(ctx).value)
	
end Program
