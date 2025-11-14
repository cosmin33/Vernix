package io.vernix

import scala.collection.mutable

opaque type VarHeap = List[mutable.Map[String, VarHeap.Value]]
object VarHeap:
	def empty: VarHeap = List(mutable.Map.empty)
	case class Value(typeName: String, var value: Any)

	extension (self: VarHeap)
		def nest(): VarHeap = mutable.Map.empty[String, VarHeap.Value] :: self
		def unNest(): VarHeap =
			self match
				case Nil => Nil
				case list @ _ :: Nil => list
				case _ :: tail => tail
		def getVariable[A: Type](name: String): A =
			var these = self
			while these.nonEmpty do
				these.head.get(name) match
					case Some(Value(t, v)) if t == Type[A].name => return v.asInstanceOf[A]
					case Some(Value(t, _)) => throw new ClassCastException(s"Variable $name is of type $t, expected ${Type[A].name}")
					case None => these = these.tail
			throw new NoSuchElementException(s"Variable $name not found")
		def addVariable[A: Type](name: String, value: A): Unit =
			self match
				case Nil => throw new IllegalStateException("No scope to add variable to")
				case head :: _ =>
					if head.contains(name) then
						throw new IllegalArgumentException(s"Variable $name already exists in the current scope")
					else
						head.addOne(name, Value(Type[A].name, value))
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
end VarHeap

opaque type VarCtx = List[mutable.Map[String, String]] // variable name -> type name
object VarCtx:
	def empty: VarCtx = List(mutable.Map.empty)

	extension(self: VarCtx)
		def nest(): VarCtx = mutable.Map.empty[String, String] :: self
		def unNest(): VarCtx =
			self match
				case Nil => Nil
				case list @ _ :: Nil => list
				case _ :: tail => tail
		def addVariable(name: String, typeName: String): Unit =
			self match
				case Nil => throw new IllegalStateException("No scope to add variable to")
				case head :: _ =>
					if head.contains(name) then
						throw new IllegalArgumentException(s"Variable $name already exists in the current scope")
					else
						head.addOne(name, typeName)
		def getVariableType(name: String): Option[String] =
			var these = self
			while these.nonEmpty do
				these.head.get(name) match
					case Some(t) => return Some(t)
					case None => these = these.tail
			None
end VarCtx
