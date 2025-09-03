package io.vernix

import scala.util.Try
import zio.*

trait Ops[F[_]]:
	def typeK: TypeK[F]
	def value[A](v: A): F[A]
	def add(l: F[Int], r: F[Int]): F[Int]
	def mul(l: F[Int], r: F[Int]): F[Int]
	def concat(l: F[String], r: F[String]): F[String]
	def len(fa:F[String]): F[Int]
	def repeatUntil[A](action: F[A])(condition: F[Boolean]): F[A]
	def doWhile[A](condition: F[Boolean])(action: F[A]): F[Unit]
	def ifElse[A](cond: F[Boolean])(ifTrue: F[A], ifFalse: F[A]): F[A]
	def leftEntuple[A, T <: NonEmptyTuple](a: F[A], t: F[T]): F[A *: T]
	def rightEntuple[T <: NonEmptyTuple, A](t: F[T], a: F[A]): F[Tuple.Append[T, A]]
	//def size[L[x] <: IterableOnce[x], A](fa:F[L[A]]): F[Int]
	//def merge[A, B, C](fa: F[A])(left: F[MappingFunc[A, C]], right: F[MappingFunc[B, C]]): F[(B, C)]
	def *>[A, B](l: F[A], r: F[B]): F[B]
object Ops:
	def apply[F[_]](using ops: Ops[F]): Ops[F] = ops

	given Ops[Task] =
		new Ops[Task]:
			val typeK: TypeK[Task] = new TypeK[Task]:
				def name: String = "Task[_]"
			def value[A](v: A): Task[A] = ZIO.succeed(v)
			def add(l: Task[Int], r: Task[Int]): Task[Int] = l.zipWith(r)(_ + _)
			def mul(l: Task[Int], r: Task[Int]): Task[Int] = l.zipWith(r)(_ * _)
			def len(fa: Task[String]): Task[Int] = fa.map(_.length)
			def concat(l: Task[String], r: Task[String]): Task[String] = l.zipWith(r)(_ + _)
			def repeatUntil[A](action: Task[A])(condition: Task[Boolean]): Task[A] =
				action.flatMap { a =>
					condition.flatMap { cond =>
						if cond then ZIO.succeed(a)
						else repeatUntil(action)(condition)
					}
				}
			def doWhile[A](condition: Task[Boolean])(action: Task[A]): Task[Unit] =
				condition.flatMap(ZIO.unlessDiscard(_)(action *> doWhile(condition)(action)))
			def ifElse[A](cond: Task[Boolean])(ifTrue: Task[A], ifFalse: Task[A]): Task[A] =
				cond.flatMap(if _ then ifTrue else ifFalse)
			def leftEntuple[A, T <: NonEmptyTuple](a: Task[A], t: Task[T]): Task[A *: T] = a.zipWith(t)(_ *: _)
			def rightEntuple[T <: NonEmptyTuple, A](t: Task[T], a: Task[A]): Task[Tuple.Append[T, A]] = t.zipWith(a)(_ :* _)
			def *>[A, B](l: Task[A], r: Task[B]): Task[B] = l *> r

	given Ops[Try] =
		new Ops[Try]:
			val typeK: TypeK[Try] = new TypeK[Try]:
				def name: String = "Try[_]"
			def value[A](v: A): Try[A] = scala.util.Try(v)
			def add(l: Try[Int], r: Try[Int]): Try[Int] = l.flatMap(a => r.map(b => a + b))
			def mul(l: Try[Int], r: Try[Int]): Try[Int] = l.flatMap(a => r.map(b => a * b))
			def len(fa: Try[String]): Try[Int] = fa.map(_.length)
			def concat(l: Try[String], r: Try[String]): Try[String] = l.flatMap(a => r.map(b => a + b))
			def repeatUntil[A](action: Try[A])(condition: Try[Boolean]): Try[A] =
				action.flatMap { a =>
					condition.flatMap { cond =>
						if cond then Try(a)
						else repeatUntil(action)(condition)
					}
				}
			def doWhile[A](condition: Try[Boolean])(action: Try[A]): Try[Unit] =
				condition.flatMap(if _ then Try(()) else action.flatMap(_ => doWhile(condition)(action)))
			def ifElse[A](cond: Try[Boolean])(ifTrue: Try[A], ifFalse: Try[A]): Try[A] =
				cond.flatMap(if _ then ifTrue else ifFalse)
			def leftEntuple[A, T <: NonEmptyTuple](a: Try[A], t: Try[T]): Try[A *: T] = a.flatMap(a => t.map(t => a *: t))
			def rightEntuple[T <: NonEmptyTuple, A](t: Try[T], a: Try[A]): Try[Tuple.Append[T, A]] = 
				t.flatMap(t => a.map(a => t :* a))
			def *>[A, B](l: Try[A], r: Try[B]): Try[B] = l.flatMap(_ => r)

	trait OpsStr extends Ops[[a] =>> String]:
		val typeK: TypeK[[a] =>> String] = new TypeK[[a] =>> String]:
			def name: String = "[a] =>> String"
		def value[A](v: A): String = v.toString
		def add(l: String, r: String): String = s"($l + $r)"
		def mul(l: String, r: String): String = s"($l * $r)"
		def len(fa: String): String = s"$fa.len"
		def concat(l: String, r: String): String = s"($l ++ $r)"
		def repeatUntil[A](action: String)(condition: String): String = s"repeat { $action } until { $condition }"
		def doWhile[A](condition: String)(action: String): String = s"do { $action } while { $condition }"
		def ifElse[A](cond: String)(ifTrue: String, ifFalse: String): String = s"if ($cond) { $ifTrue } else { $ifFalse }"
		def leftEntuple[A, T <: NonEmptyTuple](a: String, t: String): String = s"($a, $t)"
		def rightEntuple[T <: NonEmptyTuple, A](t: String, a: String): String = s"($t, $a)"
		def *>[A, B](l: String, r: String): String = s"$l\n$r"
	given Ops[[a] =>> String] = new OpsStr {}

end Ops
