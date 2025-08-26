package io.vernix

trait Expr[A]:
	self =>
	def apply[F[_]: Ops]: F[A]
//	def mapFun[B: Type](f: MappingFun[A, A] => MappingFun[A, B]): Expr[B] = new Expr[B]:
//		def apply[F[_]: Ops]: F[B] = f(MappingFun.id)(self[F])
	def + (that: Expr[Int])(using ev: A =:= Int): Expr[Int] = new Expr[Int]:
		def apply[F[_]: Ops]: F[Int] = Ops[F].add(ev.substituteCo(self[F]), that[F])
	def * (that: Expr[Int])(using ev: A =:= Int): Expr[Int] = new Expr[Int]:
		def apply[F[_]: Ops]: F[Int] = Ops[F].mul(ev.substituteCo(self[F]), that[F])
	def ++ (that: Expr[String])(using e: A =:= String): Expr[String] = new Expr[String]:
		def apply[F[_]: Ops]: F[String] = Ops[F].concat(e.substituteCo(self[F]), that[F])
	def tuple[B: Type](that: Expr[B]): Expr[(A, B)] = new Expr[(A, B)]:
		def apply[F[_]: Ops]: F[(A, B)] = Ops[F].tuple(self[F], that[F])
	def *: [T <: NonEmptyTuple](that: Expr[T]): Expr[A *: T] = new Expr[A *: T]:
		def apply[F[_]: Ops]: F[A *: T] = Ops[F].leftEntuple(self[F], that[F])
	def :* [T <: NonEmptyTuple](that: Expr[T]): Expr[Tuple.Append[T, A]] = new Expr[Tuple.Append[T, A]]:
		def apply[F[_]: Ops]: F[Tuple.Append[T, A]] = Ops[F].rightEntuple(that[F], self[F])
	def elementN[AA <: A & Tuple, N <: Int](n: N)(using ev: A =:= AA): Expr[Tuple.Elem[AA, N]] = new Expr[Tuple.Elem[AA, N]]:
		def apply[F[_]: Ops]: F[Tuple.Elem[AA, N]] = Ops[F].elementN[AA, N](ev.substituteCo(self[F]), n)
	def map[B: Type](f: A => B): Expr[B] = new Expr[B]:
		def apply[F[_]: Ops]: F[B] = Ops[F].map(self[F])(f)
	def flatMap[B: Type](f: A => Expr[B]): Expr[B] = new Expr[B]:
		def apply[F[_]: Ops]: F[B] = Ops[F].flatMap(self[F])(a => f(a)[F])
object Expr:
	def value[A](v: A): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].value(v)
	def variable[A: Type](name: String): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].variable(name)
	def function[A: Type, B: Type](name: String, param: Expr[A]): Expr[B] = new Expr[B]:
		def apply[F[_]: Ops]: F[B] = Ops[F].function(name, param[F])
	def iif[A](test: Expr[Boolean], thenBranch: Expr[A], elseBranch: Expr[A]): Expr[A] = new Expr[A]:
		def apply[F[_]: Ops]: F[A] = Ops[F].iif(test[F], thenBranch[F], elseBranch[F])
	def eif[A, B](test: Expr[Boolean], thenBranch: Expr[A], elseBranch: Expr[B]): Expr[Either[A, B]] = new Expr[Either[A, B]]:
		def apply[F[_]: Ops]: F[Either[A, B]] = Ops[F].eif(test[F], thenBranch[F], elseBranch[F])
end Expr

trait MappingFun[A, B]:
	def apply[F[_]: Ops](v: F[A]): F[B]
object MappingFun:
	def apply[A, B](f: A => Expr[B]): MappingFun[A, B] = new MappingFun[A, B]:
		def apply[F[_]: Ops](v: F[A]): F[B] = Ops[F].flatMap(v)(f(_)[F])
	def id[A]: MappingFun[A, A] = new MappingFun[A, A]:
		def apply[F[_]: Ops](v: F[A]): F[A] = v
end MappingFun
