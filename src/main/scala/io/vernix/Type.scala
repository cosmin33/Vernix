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

	given EmptyTupleType: Type[EmptyTuple] = new Type[EmptyTuple]:
		override def name: String = "EmptyTuple"

	given NonEmptyTupleType[H, T <: Tuple](using h: Type[H], t: Type[T]): Type[H *: T] = new Type[H *: T]:
		override def name: String =
			val tn = t.name
			if tn == "EmptyTuple" then s"(${h.name})"
			else s"(${h.name}, ${tn.substring(1, tn.length - 1)})"

	given TupRightType[T <: NonEmptyTuple, A](using tt: Type[T], ta: Type[A]): Type[Tuple.Append[T, A]] = new Type[Tuple.Append[T, A]]:
		override def name: String = s"${Type[T].name} :* ${Type[A].name}"

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

	private val baseTypes: Map[String, Type[?]] =
		Map("Int" -> IntType, "Double" -> DoubleType, "Boolean" -> BooleanType, "String" -> StringType, "Unit" -> UnitType)

	private def consHelper[H, T <: Tuple](h: Type[H], t: Type[T]): Type[H *: T] = NonEmptyTupleType(using h, t)

	/** Combine an element type with a tuple type to form the cons type `H *: T`. */
	def cons(h: Type[?], t: Type[? <: Tuple]): Type[? <: Tuple] = consHelper(h, t)

	/** Build the type of a tuple from the types of its elements. */
	def tupleTypeOf(parts: List[Type[?]]): Type[? <: Tuple] =
		parts.foldRight[Type[? <: Tuple]](EmptyTupleType)((h, acc) => cons(h, acc))

	/** Split a comma-separated list, respecting nested parentheses. */
	private def splitTopLevel(s: String): List[String] =
		val parts = List.newBuilder[String]
		val current = new StringBuilder
		var depth = 0
		for c <- s do
			c match
				case '(' => depth += 1; current.append(c)
				case ')' => depth -= 1; current.append(c)
				case ',' if depth == 0 => parts += current.toString; current.clear()
				case _ => current.append(c)
		parts += current.toString
		parts.result().map(_.trim).filter(_.nonEmpty)

	/** Resolve a type name (base type or tuple of resolvable types) back to a `Type`. */
	def resolve(name: String): Option[Type[?]] =
		val s = name.trim
		baseTypes.get(s).orElse {
			if s.startsWith("(") && s.endsWith(")") then
				val resolved = splitTopLevel(s.substring(1, s.length - 1)).map(resolve)
				Option.when(resolved.nonEmpty && resolved.forall(_.isDefined))(tupleTypeOf(resolved.flatten))
			else None
		}

	/** If `t` is a tuple type, return the types of its elements. */
	def elementTypes(t: Type[?]): Option[List[Type[?]]] =
		val s = t.name.trim
		if s.startsWith("(") && s.endsWith(")") then
			val resolved = splitTopLevel(s.substring(1, s.length - 1)).map(resolve)
			Option.when(resolved.nonEmpty && resolved.forall(_.isDefined))(resolved.flatten)
		else None
end Type
