package io.vernix

trait Type[T]:
	def name: String

	type TypeOf = T
object Type:
	def apply[T](using t: Type[T]): Type[T] = t
	
	given UnitType: Type[Unit] = new Type[Unit]:
		override def name: String = "Unit"
	
	given StringType: Type[String] = new Type[String]:
		override def name: String = "String"

	given IntType: Type[Int] = new Type[Int]:
		override def name: String = "Int"

	given BooleanType: Type[Boolean] = new Type[Boolean]:
		override def name: String = "Boolean"

	given DoubleType: Type[Double] = new Type[Double]:
		override def name: String = "Double"

	given FunctionType[A: Type, B: Type]: Type[A => B] = new Type[A => B]:
		override def name: String = s"(${Type[A].name} => ${Type[B].name})"

	given Tuple2Type[A: Type, B: Type]: Type[(A, B)] = new Type[(A, B)]:
		override def name: String = s"(${Type[A].name}, ${Type[B].name})"
		
	given TupLeftType[A, T <: NonEmptyTuple](using ta: Type[A], tt: Type[T]): Type[A *: T] = new Type[A *: T]:
		override def name: String = s"(${Type[A].name} -> ${Type[T].name})"
	
	given TupRightType[T <: NonEmptyTuple, A](using tt: Type[T], ta: Type[A]): Type[Tuple.Append[T, A]] = new Type[Tuple.Append[T, A]]:
		override def name: String = s"${Type[T].name} :* ${Type[A].name}"
		
//	given TupEmptyType: Type[EmptyTuple] = new Type[EmptyTuple]:
//		override def name: String = "EmptyTuple"

	given EitherType[A: Type, B: Type]: Type[Either[A, B]] = new Type[Either[A, B]]:
		override def name: String = s"Either[${Type[A].name}, ${Type[B].name}]"
	
	given TypeType[A: Type]: Type[Type[A]] = new Type[Type[A]]:
		override def name: String = s"Type[${Type[A].name}]"

	given ListType[A: Type]: Type[List[A]] = new Type[List[A]]:
		override def name: String = s"List[${Type[A].name}]"

	given ArrayType[A: Type]: Type[Array[A]] = new Type[Array[A]]:
		override def name: String = s"Array[${Type[A].name}]"

	given OptionType[A: Type]: Type[Option[A]] = new Type[Option[A]]:
		override def name: String = s"Option[${Type[A].name}]"
end Type
