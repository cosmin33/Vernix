package io.vernix

import cats.*
import cats.data.*
import cats.implicits.*

trait Statements[F[_]] extends Ops[F]:
	def variable[A: Type](name: String): F[A]
	def let[A: Type](name: String, value: F[A]): F[A]
	def nest[A](fa: F[A]): F[A]
	def funDef[A: Type, B: Type](name: String, param: String, body: F[B]): F[Unit]
	def funCall[A: Type, B: Type](name: String, param: F[A]): F[B]
object Statements:
	def apply[F[_]](using ops: Statements[F]): Statements[F] = ops

	type CtxState[A] = State[OpContext, A]
	
	private[vernix] val stateStatements: Statements[[a] =>> CtxState[Expr[a]]] =
		new Statements[[a] =>> CtxState[Expr[a]]]:
			def typeK: TypeK[[a] =>> CtxState[Expr[a]]] = new TypeK[[a] =>> CtxState[Expr[a]]]:
				def name: String = "CtxState[Expr[_]]"
			def variable[A: Type](name: String): CtxState[Expr[A]] =
				State { ctx =>
					val v = ctx.getVariable[Expr[A]](name) match
						case OpContext.SearchResult.Found(v) => v
						case OpContext.SearchResult.NotFound =>
							throw new NoSuchElementException(s"Variable $name not found")
						case OpContext.SearchResult.TypeMismatch(expected, found) =>
							throw new ClassCastException(s"Variable $name is of type $found, expected $expected")
					(ctx, v)
				}
			def len(fa: CtxState[Expr[String]]): CtxState[Expr[Int]] = fa.map(e => e.len)
			def let[A: Type](name: String, value: CtxState[Expr[A]]): CtxState[Expr[A]] =
				State { ctx =>
					val (newCtx, v) = value.run(ctx).value
					val v1 = v.memoize
					val updatedCtx = newCtx.addVariable(name, v1)
					(updatedCtx, v1)
				}
			def nest[A](fa: CtxState[Expr[A]]): CtxState[Expr[A]] =
				State(ctx => ctx -> fa.runA(ctx).value)
			def funDef[A: Type, B: Type](name: String, param: String, body: CtxState[Expr[B]]): CtxState[Expr[Unit]] =
				State { ctx =>
					val updatedCtx = ctx.addFunction[Expr[A], Expr[B]](name, (ea: Expr[A]) => body.runA(ctx.addVariable(param, ea)).value)
					updatedCtx -> Expr.value(())
				}
			def funCall[A: Type, B: Type](name: String, param: CtxState[Expr[A]]): CtxState[Expr[B]] =
				State { ctx =>
					val f = ctx.getFunction[Expr[A], Expr[B]](name) match
						case OpContext.SearchResult.Found(f) => f
						case OpContext.SearchResult.NotFound =>
							throw new NoSuchElementException(s"Function $name not found")
						case OpContext.SearchResult.TypeMismatch(expected, found) =>
							throw new ClassCastException(s"Function $name is of type $found, expected $expected")
					ctx -> f(param.runA(ctx).value)
				}
			def value[A: Type](v: A): CtxState[Expr[A]] = State.pure(Expr.value(v))
			def add[N: {Type, Numeric}](l: CtxState[Expr[N]], r: CtxState[Expr[N]]): CtxState[Expr[N]] =
				State(ctx => (ctx, l.runA(ctx).value + r.runA(ctx).value))
			def sub[N: {Type, Numeric}](l: CtxState[Expr[N]], r: CtxState[Expr[N]]): CtxState[Expr[N]] =
				State(ctx => (ctx, l.runA(ctx).value - r.runA(ctx).value))
			def mul[N: {Type, Numeric}](l: CtxState[Expr[N]], r: CtxState[Expr[N]]): CtxState[Expr[N]] =
				State(ctx => (ctx, l.runA(ctx).value * r.runA(ctx).value))
			def div[N: {Type, Fractional}](l: CtxState[Expr[N]], r: CtxState[Expr[N]]): CtxState[Expr[N]] =
				State(ctx => (ctx, l.runA(ctx).value / r.runA(ctx).value))
			def neg[N: {Type, Numeric}](a: CtxState[Expr[N]]): CtxState[Expr[N]] =
				State(ctx => (ctx, a.runA(ctx).value.neg))
			def abs[N: {Type, Numeric}](a: CtxState[Expr[N]]): CtxState[Expr[N]] =
				State(ctx => (ctx, a.runA(ctx).value.abs))
			def concat(l: CtxState[Expr[String]], r: CtxState[Expr[String]]): CtxState[Expr[String]] =
				State(ctx => (ctx, l.runA(ctx).value ++ r.runA(ctx).value))
			def repeatUntil[A](action: CtxState[Expr[A]])(condition: CtxState[Expr[Boolean]]): CtxState[Expr[A]] =
				State(ctx => ctx -> action.runA(ctx).value.repeatUntil(condition.runA(ctx).value))
			def doWhile[A](condition: CtxState[Expr[Boolean]])(action: CtxState[Expr[A]]): CtxState[Expr[Unit]] =
				State(ctx => ctx -> Expr.doWhile(condition.runA(ctx).value)(action.runA(ctx).value))
			def ifElse[A](cond: CtxState[Expr[Boolean]])(ifTrue: CtxState[Expr[A]], ifFalse: CtxState[Expr[A]]): CtxState[Expr[A]] =
				State(ctx => ctx -> cond.runA(ctx).value.ifElse(ifTrue.runA(ctx).value, ifFalse.runA(ctx).value))
			def leftEntuple[A, T <: NonEmptyTuple](a: CtxState[Expr[A]], t: CtxState[Expr[T]]): CtxState[Expr[A *: T]] =
				State(ctx => ctx -> a.runA(ctx).value.leftEntuple(t.runA(ctx).value))
			def rightEntuple[T <: NonEmptyTuple, A](t: CtxState[Expr[T]], a: CtxState[Expr[A]]): CtxState[Expr[Tuple.Append[T, A]]] =
				State(ctx => ctx -> a.runA(ctx).value.rightEntuple(t.runA(ctx).value))
			def *>[A, B](l: CtxState[Expr[A]], r: CtxState[Expr[B]]): CtxState[Expr[B]] = l *> r

end Statements
