package io.vernix

import zio.*
import OpContext.SearchResult

trait Ops[F[_]]:
	def value[A](v: A): F[A]
	def variable[A: Type](name: String): F[A]
	def function[A: Type, B: Type](name: String, param: F[A]): F[B]
//	def mapFun[A: Type, B: Type](fa: F[A])(f: MappingFun[A, B]): F[B] = f(fa)(using this)
	def add(l: F[Int], r: F[Int]): F[Int]
	def mul(l: F[Int], r: F[Int]): F[Int]
	def concat(l: F[String], r: F[String]): F[String]
	def tuple[A, B](l: F[A], r: F[B]): F[(A, B)]
	def leftEntuple[A, T <: NonEmptyTuple](a: F[A], t: F[T]): F[A *: T]
	def rightEntuple[T <: NonEmptyTuple, A](t: F[T], a: F[A]): F[Tuple.Append[T, A]]
	def elementN[T <: Tuple, N <: Int](t: F[T], n: N): F[Tuple.Elem[T, N]]
	def iif[A](test: F[Boolean], thenBranch: F[A], elseBranch: F[A]): F[A]
	def eif[A, B](test: F[Boolean], thenBranch: F[A], elseBranch: F[B]): F[Either[A, B]]
	def map[A, B](fa: F[A])(f: A => B): F[B]
	def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
object Ops:
	def apply[F[_]](using ops: Ops[F]): Ops[F] = ops

type CTask[A] = RIO[OpContext, A]

given opsC1Taxk: Ops[CTask] =
	new Ops[CTask]:
		def value[A](v: A): CTask[A] = ZIO.succeed(v)
		def add(l: CTask[Int], r: CTask[Int]): CTask[Int] = l.zipWith(r)(_ + _)
		def mul(l: CTask[Int], r: CTask[Int]): CTask[Int] = l.zipWith(r)(_ * _)
		def concat(l: CTask[String], r: CTask[String]): CTask[String] = l.zipWith(r)(_ + _)
		def tuple[A, B](l: CTask[A], r: CTask[B]): CTask[(A, B)] = l.zipWith(r)((_, _))
		def leftEntuple[A, T <: NonEmptyTuple](a: CTask[A], t: CTask[T]): CTask[A *: T] = a.zipWith(t)(_ *: _)
		def rightEntuple[T <: NonEmptyTuple, A](t: CTask[T], a: CTask[A]): CTask[Tuple.Append[T, A]] = t.zipWith(a)(_ :* _)
		def elementN[T <: Tuple, N <: Int](t: CTask[T], n: N): CTask[Tuple.Elem[T, N]] =
			t.map(tup => tup.productElement(n).asInstanceOf[Tuple.Elem[T, N]])
		def iif[A](test: CTask[Boolean], thenBranch: CTask[A], elseBranch: CTask[A]): CTask[A] =
			test.flatMap(if _ then thenBranch else elseBranch)
		def eif[A, B](test: CTask[Boolean], thenBranch: CTask[A], elseBranch: CTask[B]): CTask[Either[A, B]] =
			test.flatMap(if _ then thenBranch.map(Left(_)) else elseBranch.map(Right(_)))
		def variable[A: Type](name: String): CTask[A] =
			ZIO.serviceWithZIO[OpContext] { _.getVariable[A](name) match
				case SearchResult.Found(v) => ZIO.succeed(v)
				case SearchResult.NotFound => ZIO.fail(new NoSuchElementException(s"Variable $name not found"))
				case SearchResult.TypeMismatch(expected, found) =>
					ZIO.fail(new ClassCastException(s"Variable $name is of type $found, expected $expected"))
			}
		def function[A: Type, B: Type](name: String, param: CTask[A]): CTask[B] =
			ZIO.serviceWithZIO[OpContext] { _.getFunction[A, B](name) match
				case SearchResult.Found(f) => param.flatMap(f(_)[CTask])
				case SearchResult.NotFound => ZIO.fail(new NoSuchElementException(s"Function $name not found"))
				case SearchResult.TypeMismatch(expected, found) =>
					ZIO.fail(new ClassCastException(s"Function $name is of type $found, expected $expected"))
			}
		def map[A, B](fa: CTask[A])(f: A => B): CTask[B] = fa.map(f)
		def flatMap[A, B](fa: CTask[A])(f: A => CTask[B]): CTask[B] = fa.flatMap(f)

given opsString: Ops[[a] =>> String] =
	new Ops[[a] =>> String]:
		def value[A](v: A): String = v.toString
		def add(l: String, r: String): String = s"($l + $r)"
		def mul(l: String, r: String): String = s"($l * $r)"
		def concat(l: String, r: String): String = s"\"$l\" + \"$r\""
		def tuple[A, B](l: String, r: String): String = s"($l, $r)"
		def leftEntuple[A, T <: NonEmptyTuple](a: String, t: String): String = s"($a, $t)"
		def rightEntuple[T <: NonEmptyTuple, A](t: String, a: String): String = s"($t, $a)"
		def elementN[T <: Tuple, N <: Int](t: String, n: N): String = s"$t(${n + 1})"
		def iif[A](test: String, thenBranch: String, elseBranch: String): String =
			s"if ($test) then $thenBranch else $elseBranch"
		def eif[A, B](test: String, thenBranch: String, elseBranch: String): String =
			s"if ($test) then Left($thenBranch) else Right($elseBranch)"
		def variable[A: Type](name: String): String = s"$name[${Type[A].name}]"
		def function[A: Type, B: Type](name: String, param: String): String =
			s"$name[${Type[A].name} => ${Type[B].name}]($param)"
		def map[A, B](fa: String)(f: A => B): String = s"map($fa)"
		def flatMap[A, B](fa: String)(f: A => String): String = s"flatMap($fa)"
