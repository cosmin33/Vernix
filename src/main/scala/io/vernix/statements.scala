package io.vernix

import cats.{FlatMap, Foldable, Monad, Traverse}
import cats.data.*
import cats.implicits.*
import zio.*
import zio.Console.*
import zio.interop.catz.*

import scala.util.Try

trait Statements[F[_]] extends Ops[F]:
	def variable[A: Type](name: String): F[A]
	def let[A: Type](name: String, value: F[A]): F[A]
	def nest[A](fa: F[A]): F[A]
	def funDef[A: Type, B: Type](name: String, param: String, body: F[B]): F[Unit]
	def function[A: Type, B: Type](name: String, param: F[A]): F[B]
object Statements:
	def apply[F[_]](using ops: Statements[F]): Statements[F] = ops
	
	private trait StatementsStr extends Statements[[a] =>> String] with OpsStr:
		def variable[A: Type](name: String): String = s"var[${Type[A].name}]($name)"
		def let[A: Type](name: String, value: String): String = s"let[${Type[A].name}] $name = $value"
		def nest[A](fa: String): String = s"{$fa}"
		def funDef[A: Type, B: Type](name: String, param: String, body: String): String =
			s"def $name(${Type[A].name} $param): ${Type[B].name} = $body"
		def function[A: Type, B: Type](name: String, param: String): String =
			s"$name[${Type[A].name} => ${Type[B].name}]($param)"
	given Statements[[a] =>> String] = new StatementsStr {}
	
	private[vernix] val stateStatements: Statements[[a] =>> State[OpContext, Expr[a]]] =
	new Statements[[a] =>> State[OpContext, Expr[a]]]:
		def typeK: TypeK[[a] =>> State[OpContext, Expr[a]]] = new TypeK[[a] =>> State[OpContext, Expr[a]]]:
			def name: String = "State[OpContext, Expr[_]]"
		def variable[A: Type](name: String): State[OpContext, Expr[A]] =
			State { ctx =>
				val v = ctx.getVariable[Expr[A]](name) match
					case OpContext.SearchResult.Found(v) => v
					case OpContext.SearchResult.NotFound =>
						throw new NoSuchElementException(s"Variable $name not found")
					case OpContext.SearchResult.TypeMismatch(expected, found) =>
						throw new ClassCastException(s"Variable $name is of type $found, expected $expected")
				(ctx, v)
			}
		def len(fa: State[OpContext, Expr[String]]): State[OpContext, Expr[Int]] = fa.map(e => e.len)
		def let[A: Type](name: String, value: State[OpContext, Expr[A]]): State[OpContext, Expr[A]] =
			State { ctx =>
				val (newCtx, v) = value.run(ctx).value
				val v1 = v.memoize
				val updatedCtx = newCtx.addVariable(name, v1)
				(updatedCtx, v1)
			}
		def nest[A](fa: State[OpContext, Expr[A]]): State[OpContext, Expr[A]] =
			State(ctx => ctx -> fa.runA(ctx).value)
		def funDef[A: Type, B: Type](name: String, param: String, body: State[OpContext, Expr[B]]): State[OpContext, Expr[Unit]] =
			State { ctx =>
				val updatedCtx = ctx.addFunction[Expr[A], Expr[B]](name, (ea: Expr[A]) => body.runA(ctx.addVariable(param, ea)).value)
				updatedCtx -> Expr.value(())
			}
		def function[A: Type, B: Type](name: String, param: State[OpContext, Expr[A]]): State[OpContext, Expr[B]] =
			State { ctx =>
				val f = ctx.getFunction[Expr[A], Expr[B]](name) match
					case OpContext.SearchResult.Found(f) => f
					case OpContext.SearchResult.NotFound =>
						throw new NoSuchElementException(s"Function $name not found")
					case OpContext.SearchResult.TypeMismatch(expected, found) =>
						throw new ClassCastException(s"Function $name is of type $found, expected $expected")
				ctx -> f(param.runA(ctx).value)
			}
		def value[A](v: A): State[OpContext, Expr[A]] = State.pure(Expr.value(v))
		def add(l: State[OpContext, Expr[Int]], r: State[OpContext, Expr[Int]]): State[OpContext, Expr[Int]] =
			State(ctx => (ctx, l.runA(ctx).value + r.runA(ctx).value))
		def mul(l: State[OpContext, Expr[Int]], r: State[OpContext, Expr[Int]]): State[OpContext, Expr[Int]] =
			State(ctx => (ctx, l.runA(ctx).value * r.runA(ctx).value))
		def concat(l: State[OpContext, Expr[String]], r: State[OpContext, Expr[String]]): State[OpContext, Expr[String]] =
			State(ctx => (ctx, l.runA(ctx).value ++ r.runA(ctx).value))
		def *>[A, B](l: State[OpContext, Expr[A]], r: State[OpContext, Expr[B]]): State[OpContext, Expr[B]] = l *> r

end Statements
