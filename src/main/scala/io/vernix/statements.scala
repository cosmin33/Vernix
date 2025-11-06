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
	def nest[A](fa: F[A]): F[A]
object Statements:
	def apply[F[_]](using ops: Statements[F]): Statements[F] = ops

	type StatefulCtx[F[_]] = Stateful[F, VarHeap]
	given varOpsF[F[_]: {MonadThrow, StatefulCtx}]: Statements[F] = new Statements[F]:
		val S: StatefulCtx[F] = summon
		def variable[A: Type](name: String): F[A] =
			S.get.flatMap:
				c =>
				MonadError[F, Throwable].catchNonFatal:
					c.getVariable[A](name)
		def addVar[A: Type](name: String, value: F[A]): F[Unit] =
			MonadError[F, Throwable].catchNonFatal:
				value.flatMap(v => S.inspect(_.addVariable[A](name, v)))
		def setVar[A: Type](name: String, value: F[A]): F[Unit] =
			MonadError[F, Throwable].catchNonFatal:
				value.flatMap(v => S.inspect(_.setVariable[A](name, v)))
		def nest[A](fa: F[A]): F[A] =
			for {
				_ <- S.inspect(_.nest())
				a <- fa
				_ <- S.inspect(_.unNest())
			} yield a

	def getStatement[F[_]: MonadThrow](initialHeap: VarHeap): Statements[F] =
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

	given Statements[Type] = new Statements[Type]:
		def variable[A: Type](name: String): Type[A] = Type[A]
		def addVar[A: Type](name: String, value: Type[A]): Type[Unit] = Type[Unit]
		def setVar[A: Type](name: String, value: Type[A]): Type[Unit] = Type[Unit]
		def nest[A](fa: Type[A]): Type[A] = fa

end Statements
