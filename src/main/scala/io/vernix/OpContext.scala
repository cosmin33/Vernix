package io.vernix

opaque type OpContext = List[Map[String, OpContext.Value]]
object OpContext:
	def empty: OpContext = List(Map.empty)

	case class Value(typeName: String, var value: Any)

	enum SearchResult[+A]:
		case Found(value: A) extends SearchResult[A]
		case NotFound extends SearchResult[Nothing]
		case TypeMismatch(expected: String, found: String) extends SearchResult[Nothing]

	extension (self: OpContext)
		def nest: OpContext = Map.empty :: self
		def unNest: OpContext =
			self match
				case Nil => Nil
				case list @ _ :: Nil => list
				case _ :: tail => tail
		def getVariable[A: Type](name: String): SearchResult[A] =
			var these = self
			while these.nonEmpty do
				these.head.get(name) match
					case Some(Value(t, v)) if t == Type[A].name => return SearchResult.Found(v.asInstanceOf[A])
					case Some(Value(t, _)) => return SearchResult.TypeMismatch(Type[A].name, t)
					case None => these = these.tail
			SearchResult.NotFound
		def exists[A: Type](name: String): Boolean =
			var these = self
			while these.nonEmpty do
				these.head.get(name) match
					case Some(Value(t, _)) if t == Type[A].name => return true
					case Some(_) => return false
					case None => these = these.tail
			false
		/** add variable if it doesn't exist in the current (top) scope, otherwise throw */
		def addVariable[A: Type](name: String, value: A): OpContext =
			self match
				case Nil => throw new IllegalStateException("No scope to add variable to")
				case head :: tail =>
					if head.contains(name) then
						throw new IllegalArgumentException(s"Variable $name already exists in the current scope")
					else
						head.updated(name, Value(Type[A].name, value)) :: tail
		def setVariable[A: Type](name: String, value: A): Unit =
			var these = self
			while these.nonEmpty do
				these.head.get(name) match
					case Some(v) if v.typeName == Type[A].name =>
						v.value = value
						return
					case Some(v) =>
						throw new ClassCastException(s"Variable $name is of type ${v.typeName}, expected ${Type[A].name}")
					case None => these = these.tail
			throw new NoSuchElementException(s"Variable $name not found")

end OpContext
