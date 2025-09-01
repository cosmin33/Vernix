package io.vernix

trait Type[T]:
	def name: String

	type TypeOf = T
object Type:
	def apply[T](using t: Type[T]): Type[T] = t

	given StringType: Type[String] = new Type[String]:
		override def name: String = "String"

	given IntType: Type[Int] = new Type[Int]:
		override def name: String = "Int"

	given BooleanType: Type[Boolean] = new Type[Boolean]:
		override def name: String = "Boolean"

	given DoubleType: Type[Double] = new Type[Double]:
		override def name: String = "Double"

	given ExprType[A: Type]: Type[Expr[A]] = new Type[Expr[A]]:
		override def name: String = s"Expr[${Type[A].name}]"

	given FunctionType[A: Type, B: Type]: Type[A => B] = new Type[A => B]:
		override def name: String = s"(${Type[A].name} => ${Type[B].name})"

	given Tuple2Type[A: Type, B: Type]: Type[(A, B)] = new Type[(A, B)]:
		override def name: String = s"(${Type[A].name}, ${Type[B].name})"

	given EitherType[A: Type, B: Type]: Type[Either[A, B]] = new Type[Either[A, B]]:
		override def name: String = s"Either[${Type[A].name}, ${Type[B].name}]"

	given ListType[A: Type]: Type[List[A]] = new Type[List[A]]:
		override def name: String = s"List[${Type[A].name}]"

	given ArrayType[A: Type]: Type[Array[A]] = new Type[Array[A]]:
		override def name: String = s"Array[${Type[A].name}]"

	given OptionType[A: Type]: Type[Option[A]] = new Type[Option[A]]:
		override def name: String = s"Option[${Type[A].name}]"
end Type

trait TypeK[F[_]]:
	def name: String
	type TypeOf[a] = F[a]
