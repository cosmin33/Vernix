package io.vernix

import cats.{FlatMap, Foldable, Monad, Traverse}
import cats.data.*
import cats.implicits.*
import zio.*
import zio.Console.*
import zio.interop.catz.*

trait Statements[F[_]]:
	def expr[A](e: Expr[A]): F[A]
	def inner[A](s: F[A]): F[A]
	def let[A: Type](name: String, value: Expr[A]): F[A]
	def fundef[A: Type, B: Type](name: String, fun: A => Expr[B]): F[Unit]
	def repeatUntil[A](action: F[A], condition: A => F[Boolean]): F[A] =
		flatMap(action) { a =>
			flatMap(condition(a)) { cond =>
				if cond then
					expr(Expr.value(a))
				else
					inner(repeatUntil(action, condition))
			}
		}
	def whileDo[A](condition: F[Boolean], action: F[A]): F[Unit] =
		flatMap(condition) { cond =>
			if cond then
				flatMap(action) { _ =>
					inner(whileDo(condition, action))
				}
			else
				expr(Expr.value(()))
		}
	def forDo(init: F[Int], condition: Int => F[Boolean], increment: Int => F[Int])(action: Int => F[Unit]): F[Unit] =
		flatMap(init) { i =>
			flatMap(condition(i)) { cond =>
				if cond then
					flatMap(action(i)) { _ =>
						flatMap(increment(i)) { ni =>
							inner(forDo(expr(Expr.value(ni)), condition, increment)(action))
						}
					}
				else
					expr(Expr.value(()))
			}
		}
	def map[A, B](fa: F[A])(f: A => B): F[B]
	def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
object Statements:
	def apply[F[_]](using s: Statements[F]): Statements[F] = s

	given statementsS1Task: Statements[STask] =
		new Statements[STask]:
			def expr[A](e: Expr[A]): STask[A] = StateT:
				ctx => e[CTask].provideEnvironment(ZEnvironment(ctx)).map(a => (ctx, a))
			def inner[A](s: STask[A]): STask[A] =
				StateT:
					ctx => s.run(ctx).map { case (newCtx, a) => ctx -> a }
			def let[A: Type](name: String, value: Expr[A]): STask[A] =
				StateT:
					ctx => value[CTask].provideEnvironment(ZEnvironment(ctx)).map(v => ctx.addVariable[A](name, v) -> v)
			def fundef[A: Type, B: Type](name: String, fun: A => Expr[B]): STask[Unit] =
				StateT:
					ctx => ZIO.succeed(ctx.addFunction[A, B](name, fun) -> ())
			def map[A, B](fa: STask[A])(f: A => B): STask[B] = fa.map(f)
			def flatMap[A, B](fa: STask[A])(f: A => STask[B]): STask[B] = fa.flatMap(f)

	given statementString: Statements[[a] =>> String] =
		new Statements[[a] =>> String]:
			def expr[A](e: Expr[A]): String = e[[a] =>> String]
			def inner[A](s: String): String = s
			def let[A: Type](name: String, value: Expr[A]): String =
				s"let $name = ${value[[a] =>> String]}"
			def fundef[A: Type, B: Type](name: String, fun: A => Expr[B]): String =
				s"def $name: ${Type[A].name} => ${Type[B].name}"
			def map[A, B](fa: String)(f: A => B): String = s"map($fa)(...)"
			def flatMap[A, B](fa: String)(f: A => String): String = s"flatMap($fa)(...)"
end Statements

type STask[A] = StateT[Task, OpContext, A]

trait Program[A]:
	self =>
	def apply[F[_]: Statements]: F[A]
	def map[B](f: A => B): Program[B] =
		new Program[B]:
			def apply[F[_]: Statements]: F[B] = Statements[F].map(self[F])(f)
	def flatMap[B](f: A => Program[B]): Program[B] =
		new Program[B]:
			def apply[F[_]: Statements]: F[B] = Statements[F].flatMap(self[F])(a => f(a)[F])
object Program:
	def apply[A](v: A): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].expr(Expr.value(v))
	def expr[A](e: Expr[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].expr(e)
	def inner[A](p: Program[A]): Program[A] = new Program[A]:
		def apply[F[_]: Statements]: F[A] = Statements[F].inner(p[F])
	def let[A: Type](name: String, value: Expr[A]): Program[A] =
		new Program[A]:
			def apply[F[_]: Statements]: F[A] = Statements[F].let(name, value)
	def fundef[A: Type, B: Type](name: String, fun: A => Expr[B]): Program[Unit] =
		new Program[Unit]:
			def apply[F[_]: Statements]: F[Unit] = Statements[F].fundef(name, fun)

	given Monad[Program] with
		def pure[A](x: A): Program[A] = Program(x)
		override def map[A, B](fa: Program[A])(f: A => B): Program[B] =
			new Program[B]:
				def apply[F[_]: Statements]: F[B] = Statements[F].map(fa[F])(f)
		def flatMap[A, B](fa: Program[A])(f: A => Program[B]): Program[B] =
			new Program[B]:
				def apply[F[_]: Statements]: F[B] = Statements[F].flatMap(fa[F])(a => f(a)[F])
		def tailRecM[A, B](a: A)(f: A => Program[Either[A, B]]): Program[B] =
			new Program[B]:
				def apply[F[_]: Statements]: F[B] = Statements[F].flatMap(f(a)[F]) {
					case Left(na) => tailRecM(na)(f)[F]
					case Right(b) => Statements[F].expr(Expr.value(b))
				}
end Program
