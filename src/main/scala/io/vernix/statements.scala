package io.vernix

import cats.{Apply, Defer, Monad, MonadError, MonadThrow}
import scala.util.Try
import zio.*
import cats.data.*
import cats.implicits.*
import cats.mtl.Stateful
import zio.interop.catz.*

trait Statements[F[_]]:
	def variable[A: Type](name: String): F[A]
	def addVar[A: Type](name: String, value: F[A]): F[Unit]
	def setVar[A: Type](name: String, value: F[A]): F[Unit]
	def addFunction[A: Type, B: Type](name: String, argName: String, body: F[B]): F[Unit]
	def callFunction[A: Type, B: Type](name: String, arg: F[A]): F[B]
	def nest[A](fa: F[A]): F[A]
object Statements:
	def apply[F[_]](using ops: Statements[F]): Statements[F] = ops

	case class Fun[F[_], A, B](f: A => F[B])
	object Fun:
		given[F[_], A, B](using Type[A], Type[B]): Type[Fun[F, A, B]] = new Type[Fun[F, A, B]]:
			def name: String = s"Fun[F, ${Type[A].name}, ${Type[B].name}]"

	type StatefulCtx[F[_]] = Stateful[F, VarHeap]
	given varOpsF[F[_]: {MonadThrow, Defer, StatefulCtx}]: Statements[F] = new Statements[F]:
		val S: StatefulCtx[F] = summon
		def variable[A: Type](name: String): F[A] =
			Defer[F].defer:
				S.get >>= (c => MonadThrow[F].catchNonFatal(c.getVariable[A](name)))
		def addVar[A: Type](name: String, value: F[A]): F[Unit] =
			Defer[F].defer:
				value >>= (v => S.get >>= (vh => MonadThrow[F].catchNonFatal(vh.addVariable[A](name, v))))
		def setVar[A: Type](name: String, value: F[A]): F[Unit] =
			Defer[F].defer:
				value >>= (v => S.get >>= (vh => MonadThrow[F].catchNonFatal(vh.setVariable[A](name, v))))
		def nest[A](fa: F[A]): F[A] =
			Defer[F].defer(S.modify(_.nest())).*>(fa).flatTap(_ => Defer[F].defer(S.modify(_.unNest())))
		def addFunction[A: Type, B: Type](name: String, argName: String, body: F[B]): F[Unit] =
			val ff: A => F[B] =
				a => for {
					_ <- Defer[F].defer(S.modify(_.nest()))
					_ <- Defer[F].defer(S.get >>= (vh => MonadThrow[F].catchNonFatal(vh.addVariable[A](argName, a))))
					b <- body
					_ <- Defer[F].defer(S.modify(_.unNest()))
				} yield b
			().pure[F].flatTap(_ => S.inspect(_.addVariable(name, Fun(ff))))
		def callFunction[A: Type, B: Type](name: String, arg: F[A]): F[B] =
			for {
				vh  <- S.get
				fun <- Defer[F].defer(MonadThrow[F].catchNonFatal(vh.getVariable[Fun[F, A, B]](name)))
				a   <- arg
				b   <- fun.f(a)
			} yield b

	def getStatement[F[_]: {MonadThrow, Defer}](initialHeap: VarHeap = VarHeap.empty): Statements[F] =
		var heap = initialHeap 
		given S: StatefulCtx[F] = new Stateful[F, VarHeap]:
			def monad: Monad[F] = summon
			def get: F[VarHeap] = Monad[F].pure(heap)
			def set(s: VarHeap): F[Unit] = Monad[F].pure {heap = s}
		summon

	given Statements[[a] =>> String] = new Statements[[a] =>> String]:
		def variable[A: Type](name: String): String = s"$name"
		def addVar[A: Type](name: String, value: String): String = s"var $name: ${Type[A].name} = $value"
		def setVar[A: Type](name: String, value: String): String = s"$name = $value"
		def nest[A](fa: String): String = s"{$fa}"
		def addFunction[A: Type, B: Type](name: String, argName: String, body: String): String =
			s"""
			|def $name($argName: ${Type[A].name}): ${Type[B].name} = {
			|	$body
			|}
			""".stripMargin
		def callFunction[A: Type, B: Type](name: String, arg: String): String = s"$name($arg)"


	given Statements[Type] = new Statements[Type]:
		def variable[A: Type](name: String): Type[A] = Type[A]
		def addVar[A: Type](name: String, value: Type[A]): Type[Unit] = Type[Unit]
		def setVar[A: Type](name: String, value: Type[A]): Type[Unit] = Type[Unit]
		def nest[A](fa: Type[A]): Type[A] = fa
		def addFunction[A: Type, B: Type](name: String, argName: String, body: Type[B]): Type[Unit] = Type[Unit]
		def callFunction[A: Type, B: Type](name: String, arg: Type[A]): Type[B] = Type[B]

end Statements
