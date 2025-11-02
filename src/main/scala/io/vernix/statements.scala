package io.vernix

import cats.*
import cats.data.*
import cats.implicits.*

trait Statements[F[_]] extends Ops[F]:
	def variable[A: Type](name: String): F[A]
	def addVar[A: Type](name: String, value: F[A]): F[A]
	def setVar[A: Type](name: String, value: F[A]): F[A]
	def nest[A](fa: F[A]): F[A]
	def funDef[A: Type, B: Type](name: String, param: String, body: F[B]): F[Unit]
	def funCall[A: Type, B: Type](name: String, param: F[A]): F[B]
object Statements:
	def apply[F[_]](using ops: Statements[F]): Statements[F] = ops

	type CtxState[A] = State[OpContext, A]
	
	given stateStatements: Statements[[a] =>> CtxState[Expr[a]]] =
		new Statements[[a] =>> CtxState[Expr[a]]]:
			def typeK: TypeK[[a] =>> CtxState[Expr[a]]] = new TypeK[[a] =>> CtxState[Expr[a]]]:
				def name: String = "CtxState[Expr[_]]"
			def variable[A: Type](name: String): CtxState[Expr[A]] =
				State.inspect:
					_.getVariable[Expr[A]](name) match
						case OpContext.SearchResult.Found(v) => v
						case OpContext.SearchResult.NotFound =>
							throw new NoSuchElementException(s"Variable $name not found")
						case OpContext.SearchResult.TypeMismatch(expected, found) =>
							throw new ClassCastException(s"Variable $name is of type $found, expected $expected")
			def addVar[A: Type](name: String, value: CtxState[Expr[A]]): CtxState[Expr[A]] =
				for
					v <- value
//					vm = v.memoize
//					_ <- State.modify[OpContext](_.addVariable(name, vm))
					_ <- State.modify[OpContext](_.addVariable(name, v))
				yield v
			def setVar[A: Type](name: String, value: CtxState[Expr[A]]): CtxState[Expr[A]] =
				for
					v <- value
					_ <- State.inspect[OpContext, Unit](_.setVariable(name, v))
				yield v
			def nest[A](fa: CtxState[Expr[A]]): CtxState[Expr[A]] =
				State.inspect(ctx => fa.runA(ctx.nest).value)
			def funDef[A: Type, B: Type](name: String, param: String, body: CtxState[Expr[B]]): CtxState[Expr[Unit]] =
				for
					_ <- State.modify[OpContext](_.nest)
					f <- State.inspect[OpContext, Expr[A] => Expr[B]](c => ea => body.runA(c.addVariable(param, ea)).value)
					_ <- State.modify[OpContext](_.unNest.addVariable[Expr[A] => Expr[B]](name, f))
				yield Expr.value(())
			def funCall[A: Type, B: Type](name: String, param: CtxState[Expr[A]]): CtxState[Expr[B]] =
				State.inspect:
					ctx =>
					val f = ctx.getVariable[Expr[A] => Expr[B]](name) match
						case OpContext.SearchResult.Found(f) => f
						case OpContext.SearchResult.NotFound =>
							throw new NoSuchElementException(s"Function $name not found")
						case OpContext.SearchResult.TypeMismatch(expected, found) =>
							throw new ClassCastException(s"Function $name is of type $found, expected $expected")
					f(param.runA(ctx).value)
			def value[A: Type](v: A): CtxState[Expr[A]] =
				State.pure(Expr.value(v))
			def len(fa: CtxState[Expr[String]]): CtxState[Expr[Int]] =
				fa.map(_.len)
			def toDouble(fa: CtxState[Expr[Int]]): CtxState[Expr[Double]] =
				fa.map(_.toDouble)
			def add[N: {Type, Numeric}](l: CtxState[Expr[N]], r: CtxState[Expr[N]]): CtxState[Expr[N]] =
				Apply[CtxState].map2(l, r)(_ + _)
			def sub[N: {Type, Numeric}](l: CtxState[Expr[N]], r: CtxState[Expr[N]]): CtxState[Expr[N]] =
				Apply[CtxState].map2(l, r)(_ - _)
			def mul[N: {Type, Numeric}](l: CtxState[Expr[N]], r: CtxState[Expr[N]]): CtxState[Expr[N]] =
				Apply[CtxState].map2(l, r)(_ * _)
			def div[N: {Type, Fractional}](l: CtxState[Expr[N]], r: CtxState[Expr[N]]): CtxState[Expr[N]] =
				Apply[CtxState].map2(l, r)(_ / _)
			def quot[N: {Type, Integral}](l: CtxState[Expr[N]], r: CtxState[Expr[N]]): CtxState[Expr[N]] =
				Apply[CtxState].map2(l, r)(_ `quot` _)
			def mod[N: {Type, Integral}](l: CtxState[Expr[N]], r: CtxState[Expr[N]]): CtxState[Expr[N]] =
				Apply[CtxState].map2(l, r)(_ % _)
			def neg[N: {Type, Numeric}](a: CtxState[Expr[N]]): CtxState[Expr[N]] =
				a.map(_.neg)
			def abs[N: {Type, Numeric}](a: CtxState[Expr[N]]): CtxState[Expr[N]] =
				a.map(_.abs)
			def concat(l: CtxState[Expr[String]], r: CtxState[Expr[String]]): CtxState[Expr[String]] =
				Apply[CtxState].map2(l, r)(_ ++ _)
			def repeatUntil[A](action: CtxState[Expr[A]])(condition: CtxState[Expr[Boolean]]): CtxState[Expr[A]] =
				Apply[CtxState].map2(action, condition)(_ `repeatUntil` _)
			def whileDo[A](condition: CtxState[Expr[Boolean]])(action: CtxState[Expr[A]]): CtxState[Expr[Unit]] =
				Apply[CtxState].map2(condition, action)(Expr.whileDo(_)(_))
			def ifElse[A](cond: CtxState[Expr[Boolean]])(ifTrue: CtxState[Expr[A]], ifFalse: CtxState[Expr[A]]): CtxState[Expr[A]] =
				Apply[CtxState].map3(cond, ifTrue, ifFalse)(_.ifElse(_, _))
			def and(l: CtxState[Expr[Boolean]], r: CtxState[Expr[Boolean]]): CtxState[Expr[Boolean]] =
				Apply[CtxState].map2(l, r)(_ && _)
			def or(l: CtxState[Expr[Boolean]], r: CtxState[Expr[Boolean]]): CtxState[Expr[Boolean]] =
				Apply[CtxState].map2(l, r)(_ || _)
			def not(a: CtxState[Expr[Boolean]]): CtxState[Expr[Boolean]] =
				a.map(!_)
			def equals[A: Type](l: CtxState[Expr[A]], r: CtxState[Expr[A]]): CtxState[Expr[Boolean]] =
				Apply[CtxState].map2(l, r)(_ === _)
			def notEquals[A: Type](l: CtxState[Expr[A]], r: CtxState[Expr[A]]): CtxState[Expr[Boolean]] =
				Apply[CtxState].map2(l, r)(_ !== _)
			def < [A: {Type, Ordering}](l: CtxState[Expr[A]], r: CtxState[Expr[A]]): CtxState[Expr[Boolean]] =
				Apply[CtxState].map2(l, r)(_ < _)
			def <=[A: {Type, Ordering}](l: CtxState[Expr[A]], r: CtxState[Expr[A]]): CtxState[Expr[Boolean]] =
				Apply[CtxState].map2(l, r)(_ <= _)
			def > [A: {Type, Ordering}](l: CtxState[Expr[A]], r: CtxState[Expr[A]]): CtxState[Expr[Boolean]] =
				Apply[CtxState].map2(l, r)(_ > _)
			def >=[A: {Type, Ordering}](l: CtxState[Expr[A]], r: CtxState[Expr[A]]): CtxState[Expr[Boolean]] =
				Apply[CtxState].map2(l, r)(_ >= _)
			def leftEntuple[A, T <: NonEmptyTuple](a: CtxState[Expr[A]], t: CtxState[Expr[T]]): CtxState[Expr[A *: T]] =
				Apply[CtxState].map2(a, t)(_ `leftEntuple` _)
			def rightEntuple[T <: NonEmptyTuple, A](t: CtxState[Expr[T]], a: CtxState[Expr[A]]): CtxState[Expr[Tuple.Append[T, A]]] =
				Apply[CtxState].map2(a, t)(_ `rightEntuple` _)
			def *>[A, B](l: CtxState[Expr[A]], r: CtxState[Expr[B]]): CtxState[Expr[B]] =
				Apply[CtxState].map2(l, r)(_ *> _)

end Statements
