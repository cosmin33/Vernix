package io.vernix

import scala.util.Try
import zio.*
import OpContext.SearchResult

trait Ops[F[_]]:
	def typeK: TypeK[F]
	def value[A](v: A): F[A]
	def add(l: F[Int], r: F[Int]): F[Int]
	def mul(l: F[Int], r: F[Int]): F[Int]
	def concat(l: F[String], r: F[String]): F[String]
	//def _1 [A, B](fa: F[(A, B)]): F[A]
	//def _2 [A, B](fa: F[(A, B)]): F[B]
	def len(fa:F[String]): F[Int]
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
		def *>[A, B](l: Try[A], r: Try[B]): Try[B] = l.flatMap(_ => r)

trait OpsStr extends Ops[[a] =>> String]:
	val typeK: TypeK[[a] =>> String] = new TypeK[[a] =>> String]:
		def name: String = "[a] =>> String"
	def value[A](v: A): String = v.toString
	def add(l: String, r: String): String = s"($l + $r)"
	def mul(l: String, r: String): String = s"($l * $r)"
	def len(fa: String): String = s"$fa.len"
	def concat(l: String, r: String): String = s"($l ++ $r)"
	def *>[A, B](l: String, r: String): String = s"$l\n$r"
given Ops[[a] =>> String] = new OpsStr {}
