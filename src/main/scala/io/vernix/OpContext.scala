package io.vernix

case class OpContext(vars: Map[String, (String, Any)], funs: Map[String, (String, Any)]) {
	def getVariable[A: Type](name: String): OpContext.SearchResult[A] =
		vars.get(name) match
			case Some((t, v)) if t == Type[A].name => OpContext.SearchResult.Found(v.asInstanceOf[A])
			case Some((t, _)) => OpContext.SearchResult.TypeMismatch(Type[A].name, t)
			case None => OpContext.SearchResult.NotFound
	def getFunction[A: Type, B: Type](name: String): OpContext.SearchResult[A => B] =
		funs.get(name) match
			case Some((t, f)) if t == Type[A => B].name => OpContext.SearchResult.Found(f.asInstanceOf[A => B])
			case Some((t, _)) => OpContext.SearchResult.TypeMismatch(Type[A => B].name, t)
			case None => OpContext.SearchResult.NotFound
	def addVariable[A: Type](name: String, value: A): OpContext =
		copy(vars = vars.updated(name, Type[A].name -> value))
	def addFunction[A: Type, B: Type](name: String, fn: A => B): OpContext =
		copy(funs = funs.updated(name, Type[A => B].name -> fn))
}
object OpContext:
	def empty: OpContext = OpContext(Map(), Map())
	enum SearchResult[+A]:
		case Found(value: A) extends SearchResult[A]
		case NotFound extends SearchResult[Nothing]
		case TypeMismatch(expected: String, found: String) extends SearchResult[Nothing]
end OpContext
