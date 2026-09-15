package io.vernix

import cats.implicits.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}
import io.vernix.Prog.prog
import scala.util.Try

/** Circe codec for `Program`.
  *
  * A `Program[A]` is a tagless-final term, so encoding means interpreting it into a JSON AST
  * (`Ops`/`Statements` for the constant functor `[a] =>> Json`) and decoding means rebuilding a
  * `Prog` from that AST, exactly as `Parsing`/`ParsingOps` rebuild one from source text.
  *
  * Nodes carry a `type` field only where the decoder cannot derive the type from the operands
  * (literals, variables, tuple access, function signatures); everywhere else the type is
  * reconstructed bottom-up.
  *
  * `Program`'s companion has a `given` calling `programCodec`, so `Codec[Program[A]]` is found
  * without an import.
  */
object ProgramCodec:

	/** The literal types the AST can carry: the base types, tuples of them, and — via the required
	  * codec for `A` — the program's own result type. A literal of any other type cannot be encoded,
	  * since a `Type` gives us only a name. */
	private final case class Literals(tpe: Type[?], encoder: Encoder[Any], decoder: Decoder[Any]):
		def typeName: String = tpe.name

	def programCodec[A](using encA: Encoder[A], decA: Decoder[A], typeA: Type[A]): Codec[Program[A]] =
		given Literals = Literals(typeA, encA.asInstanceOf[Encoder[Any]], decA.asInstanceOf[Decoder[Any]])
		Codec.from(
			Decoder.instance: c =>
				decodeProg(c.value).flatMap: p =>
					if p.`type`.name == typeA.name then Right(p.program.asInstanceOf[Program[A]])
					else Left(DecodingFailure(s"Program is of type ${p.`type`.name}, not ${typeA.name}", c.history)),
			Encoder.instance((p: Program[A]) => p.apply[[a] =>> Json](using jsonOps, jsonStatements))
		)

	// ---------------------------------------------------------------- encoding

	private def str(s: String): Json = Json.fromString(s)
	private def node(op: String, fields: (String, Json)*): Json = Json.obj(("op" -> str(op)) +: fields*)
	private def bin(op: String, l: Json, r: Json): Json = node(op, "l" -> l, "r" -> r)
	private def un(op: String, a: Json): Json = node(op, "a" -> a)

	private def jsonOps(using Literals): Ops[[a] =>> Json] = new Ops[[a] =>> Json]:
		def value[A: Type](v: A): Json = encodeValue(Type[A], v) match
			case Some(j) => node("value", "type" -> str(Type[A].name), "v" -> j)
			// An `Encoder` is total, so an unencodable literal becomes a node that fails to decode.
			case None => node("unencodable", "type" -> str(Type[A].name))
		def add[N: {Type, Numeric}](l: Json, r: Json): Json = bin("add", l, r)
		def sub[N: {Type, Numeric}](l: Json, r: Json): Json = bin("sub", l, r)
		def mul[N: {Type, Numeric}](l: Json, r: Json): Json = bin("mul", l, r)
		def div[N: {Type, Fractional}](l: Json, r: Json): Json = bin("div", l, r)
		def quot[N: {Type, Integral}](l: Json, r: Json): Json = bin("quot", l, r)
		def mod[N: {Type, Integral}](l: Json, r: Json): Json = bin("mod", l, r)
		def neg[N: {Type, Numeric}](a: Json): Json = un("neg", a)
		def abs[N: {Type, Numeric}](a: Json): Json = un("abs", a)
		def concat(l: Json, r: Json): Json = bin("concat", l, r)
		def len(fa: Json): Json = un("len", fa)
		def toDouble(fa: Json): Json = un("to-double", fa)
		def repeatUntil[A](action: => Json)(condition: => Json): Json =
			node("repeat-until", "action" -> action, "condition" -> condition)
		def whileDo[A](condition: => Json)(action: => Json): Json =
			node("while-do", "condition" -> condition, "action" -> action)
		def ifElse[A](cond: Json)(ifTrue: => Json, ifFalse: => Json): Json =
			node("if-else", "cond" -> cond, "if-true" -> ifTrue, "if-false" -> ifFalse)
		def and(l: Json, r: Json): Json = bin("and", l, r)
		def or(l: Json, r: Json): Json = bin("or", l, r)
		def not(a: Json): Json = un("not", a)
		def equals[A: Type](l: Json, r: Json): Json = bin("equals", l, r)
		def notEquals[A: Type](l: Json, r: Json): Json = bin("not-equals", l, r)
		def < [A: {Type, Ordering}](l: Json, r: Json): Json = bin("lt", l, r)
		def <=[A: {Type, Ordering}](l: Json, r: Json): Json = bin("lte", l, r)
		def > [A: {Type, Ordering}](l: Json, r: Json): Json = bin("gt", l, r)
		def >=[A: {Type, Ordering}](l: Json, r: Json): Json = bin("gte", l, r)
		def emptyTuple: Json = node("empty-tuple")
		def leftEntuple[A, T <: Tuple](a: Json, t: Json): Json = node("left-entuple", "a" -> a, "t" -> t)
		def rightEntuple[T <: NonEmptyTuple, A](t: Json, a: Json): Json = node("right-entuple", "t" -> t, "a" -> a)
		def tupleAt[T <: NonEmptyTuple, A: Type](t: Json, index: Int): Json =
			node("tuple-at", "type" -> str(Type[A].name), "t" -> t, "index" -> Json.fromInt(index))
		def *>[A, B](l: Json, r: Json): Json = bin("seq", l, r)

	private val jsonStatements: Statements[[a] =>> Json] = new Statements[[a] =>> Json]:
		def variable[A: Type](name: String): Json = node("variable", "type" -> str(Type[A].name), "name" -> str(name))
		def addVar[A: Type](name: String, value: Json): Json =
			node("add-var", "type" -> str(Type[A].name), "name" -> str(name), "value" -> value)
		def setVar[A: Type](name: String, value: Json): Json =
			node("set-var", "type" -> str(Type[A].name), "name" -> str(name), "value" -> value)
		def addFunction[A: Type, B: Type](name: String, argName: String, body: Json): Json =
			node("add-function", "name" -> str(name), "arg-name" -> str(argName),
				"arg-type" -> str(Type[A].name), "ret-type" -> str(Type[B].name), "body" -> body)
		def callFunction[A: Type, B: Type](name: String, arg: Json): Json =
			node("call-function", "name" -> str(name),
				"arg-type" -> str(Type[A].name), "ret-type" -> str(Type[B].name), "arg" -> arg)
		def nest[A](fa: Json): Json = node("nest", "a" -> fa)

	/** `None` for a literal whose type we have no encoder for — see `jsonOps.value`. */
	private def encodeValue(t: Type[?], v: Any)(using lit: Literals): Option[Json] =
		t.name match
			case "Int"     => Some(Json.fromInt(v.asInstanceOf[Int]))
			case "Double"  => Some(Json.fromDoubleOrString(v.asInstanceOf[Double]))
			case "Boolean" => Some(Json.fromBoolean(v.asInstanceOf[Boolean]))
			case "String"  => Some(str(v.asInstanceOf[String]))
			case "Unit"    => Some(Json.Null)
			case _ =>
				Type.elementTypes(t) match
					case Some(ts) =>
						ts.zip(v.asInstanceOf[Product].productIterator.toList)
							.traverse { case (et, ev) => encodeValue(et, ev) }
							.map(js => Json.arr(js*))
					case None => Option.when(t.name == lit.typeName)(lit.encoder(v))

	// ---------------------------------------------------------------- decoding

	private def decodeValue(t: Type[?], j: Json)(using lit: Literals): Decoder.Result[Any] =
		def fail(msg: String) = DecodingFailure(msg, Nil)
		t.name match
			case "Int"     => j.as[Int]
			case "Double"  => j.as[Double].orElse(j.as[String].flatMap(s => Try(s.toDouble).toEither.left.map(_ => fail(s"Not a Double: $s"))))
			case "Boolean" => j.as[Boolean]
			case "String"  => j.as[String]
			case "Unit"    => Right(())
			case _ =>
				Type.elementTypes(t) match
					case Some(ts) =>
						j.as[List[Json]].flatMap: js =>
							if js.size != ts.size then Left(fail(s"Expected ${ts.size} elements for ${t.name}, got ${js.size}"))
							else ts.zip(js).traverse { case (et, ej) => decodeValue(et, ej) }.map(_.foldRight[Tuple](EmptyTuple)(_ *: _))
					case None if t.name == lit.typeName => lit.decoder.decodeJson(j)
					case None => Left(fail(s"Cannot decode a literal of type ${t.name}"))

	/** `Type.resolve` knows the base types and tuples of them; the program's own result type is known
	  * only through the `Type[A]` the codec was built with. */
	private def resolveType(name: String)(using lit: Literals): Option[Type[?]] =
		Type.resolve(name).orElse(Option.when(name.trim == lit.typeName)(lit.tpe))

	private def decodeProg(j: Json)(using Literals): Decoder.Result[Prog] =
		val c = j.hcursor
		def sub(n: String): Decoder.Result[Prog] = c.downField(n).as[Json].flatMap(decodeProg)
		def tpe(n: String): Decoder.Result[Type[?]] =
			c.downField(n).as[String].flatMap(s => resolveType(s).toRight(DecodingFailure(s"Unsupported type: $s", c.history)))
		// The `Prog` builders below reject ill-typed ASTs by throwing, as `ParsingOps` does.
		def build(p: => Prog): Decoder.Result[Prog] =
			Try(p).toEither.left.map(e => DecodingFailure(s"Invalid program node: ${e.getMessage}", c.history))

		c.downField("op").as[String].flatMap:
			case "value" =>
				for
					t <- tpe("type")
					v <- c.downField("v").as[Json].flatMap(decodeValue(t, _))
				yield makeValue(t, v)
			case op @ ("add" | "sub" | "mul" | "div" | "quot" | "mod") =>
				(sub("l"), sub("r")).tupled.flatMap((l, r) => build(numOp(op, l, r)))
			case op @ ("neg" | "abs") => sub("a").flatMap(a => build(numOp1(op, a)))
			case "concat" => (sub("l"), sub("r")).tupled.flatMap((l, r) => build((l.unsafe[String] ++ r.unsafe[String]).prog))
			case "len" => sub("a").flatMap(a => build(a.unsafe[String].len.prog))
			case "to-double" => sub("a").flatMap(a => build(a.unsafe[Int].toDouble.prog))
			case "and" => (sub("l"), sub("r")).tupled.flatMap((l, r) => build(ParsingOps.op_&(l, r)))
			case "or" => (sub("l"), sub("r")).tupled.flatMap((l, r) => build(ParsingOps.op_|(l, r)))
			case "not" => sub("a").flatMap(a => build((!a.unsafe[Boolean]).prog))
			case "equals" => (sub("l"), sub("r")).tupled.flatMap((l, r) => build(ParsingOps.op_===(l, r)))
			case "not-equals" => (sub("l"), sub("r")).tupled.flatMap((l, r) => build(ParsingOps.op_!==(l, r)))
			case "lt" => (sub("l"), sub("r")).tupled.flatMap((l, r) => build(ParsingOps.op_<(l, r)))
			case "lte" => (sub("l"), sub("r")).tupled.flatMap((l, r) => build(ParsingOps.op_<=(l, r)))
			case "gt" => (sub("l"), sub("r")).tupled.flatMap((l, r) => build(ParsingOps.op_>(l, r)))
			case "gte" => (sub("l"), sub("r")).tupled.flatMap((l, r) => build(ParsingOps.op_>=(l, r)))
			case "if-else" =>
				(sub("cond"), sub("if-true"), sub("if-false")).tupled.flatMap((c, t, f) => build(ParsingOps.opIf(c, t, f)))
			case "repeat-until" =>
				(sub("action"), sub("condition")).tupled.flatMap((a, c) => build(ParsingOps.repeatUntil(a, c)))
			case "while-do" =>
				(sub("condition"), sub("action")).tupled.flatMap((c, a) => build(ParsingOps.doWhile(c, a)))
			case "seq" => (sub("l"), sub("r")).tupled.map((l, r) => l.program.*>(r.program).prog(using r.`type`))
			case "nest" => sub("a").map(_.nest)
			case "empty-tuple" => Right(Prog(Program.emptyTuple))
			case "left-entuple" => (sub("a"), sub("t")).tupled.flatMap((a, t) => build(consProg(a, t)))
			case "right-entuple" => (sub("t"), sub("a")).tupled.flatMap((t, a) => build(snocProg(t, a)))
			case "tuple-at" =>
				for
					t  <- sub("t")
					et <- tpe("type")
					i  <- c.downField("index").as[Int]
					p  <- build(tupleAtProg(t, i, et))
				yield p
			case "unencodable" =>
				c.downField("type").as[String].flatMap: t =>
					Left(DecodingFailure(s"Literal of type $t was not encodable", c.history))
			case "variable" => (tpe("type"), c.downField("name").as[String]).tupled.map(makeVariable)
			case op @ ("add-var" | "set-var") =>
				for
					t <- tpe("type")
					n <- c.downField("name").as[String]
					v <- sub("value")
					p <- build(if op == "add-var" then makeAddVar(t, n, v) else makeSetVar(t, n, v))
				yield p
			case "add-function" =>
				for
					n  <- c.downField("name").as[String]
					an <- c.downField("arg-name").as[String]
					at <- tpe("arg-type")
					rt <- tpe("ret-type")
					b  <- sub("body")
					p  <- build(makeAddFunction(at, rt, n, an, b))
				yield p
			case "call-function" =>
				for
					n   <- c.downField("name").as[String]
					at  <- tpe("arg-type")
					rt  <- tpe("ret-type")
					arg <- sub("arg")
					p   <- build(makeCallFunction(at, rt, n, arg))
				yield p
			case op => Left(DecodingFailure(s"Unknown operation: $op", c.history))

	// Builders that re-introduce the type evidence the AST only carries by name. Each one checks the
	// operand types and throws on a mismatch; `decodeProg` turns that into a `DecodingFailure`.

	private def makeValue(t: Type[?], v: Any): Prog = Prog.make(Program.value[t.TypeOf](v.asInstanceOf[t.TypeOf])(using t), t)

	private def makeVariable(t: Type[?], name: String): Prog = Prog.make(Program.variable[t.TypeOf](name)(using t), t)

	private def makeAddVar(t: Type[?], name: String, value: Prog): Prog =
		Prog(Program.addVar[t.TypeOf](name, value.unsafe[t.TypeOf](using t))(using t))

	private def makeSetVar(t: Type[?], name: String, value: Prog): Prog =
		Prog(Program.setVar[t.TypeOf](name, value.unsafe[t.TypeOf](using t))(using t))

	private def makeAddFunction(at: Type[?], rt: Type[?], name: String, argName: String, body: Prog): Prog =
		Prog(Program.addFunction[at.TypeOf, rt.TypeOf](name, argName, body.unsafe[rt.TypeOf](using rt))(using at, rt))

	private def makeCallFunction(at: Type[?], rt: Type[?], name: String, arg: Prog): Prog =
		Prog.make(Program.callFunction[at.TypeOf, rt.TypeOf](name, arg.unsafe[at.TypeOf](using at))(using at, rt), rt)

	/** Reads element `index`, checking that the operand is a tuple and the index is in range (both done
	  * by `ParsingOps.tupleAccess`) and that the declared type is the one the tuple holds there. */
	private def tupleAtProg(t: Prog, index: Int, et: Type[?]): Prog =
		val p = ParsingOps.tupleAccess(t, index)
		if p.`type`.name != et.name then
			throw new Exception(s"Element ._${index + 1} of ${t.`type`.name} is of type ${p.`type`.name}, not ${et.name}")
		p

	/** `a *: t`, typed as the cons of the two operand types (mirrors `Program.tuple`). */
	private def consProg(a: Prog, t: Prog): Prog =
		if !isTupleType(t.`type`) then throw new Exception(s"Cannot prepend to non-tuple type ${t.`type`.name}")
		val p = new Program[Tuple]:
			def apply[F[_]: {Ops, Statements}]: F[Tuple] =
				Ops[F].leftEntuple[Any, Tuple](
					a.program.asInstanceOf[Program[Any]].apply[F],
					t.program.asInstanceOf[Program[Tuple]].apply[F]
				).asInstanceOf[F[Tuple]]
		Prog.make(p, Type.cons(a.`type`, t.`type`.asInstanceOf[Type[? <: Tuple]]))

	private def isTupleType(t: Type[?]): Boolean = t.name == "EmptyTuple" || Type.elementTypes(t).isDefined

	/** `t :* a`, typed with `Type.TupRightType`. */
	private def snocProg(t: Prog, a: Prog): Prog =
		if Type.elementTypes(t.`type`).isEmpty then
			throw new Exception(s"Cannot append to non-tuple type ${t.`type`.name}")
		val p = new Program[Tuple]:
			def apply[F[_]: {Ops, Statements}]: F[Tuple] =
				Ops[F].rightEntuple[NonEmptyTuple, Any](
					t.program.asInstanceOf[Program[NonEmptyTuple]].apply[F],
					a.program.asInstanceOf[Program[Any]].apply[F]
				).asInstanceOf[  F[Tuple]]
		val tt = t.`type`.asInstanceOf[Type[NonEmptyTuple]]
		val at = a.`type`
		Prog.make(p, Type.TupRightType[NonEmptyTuple, at.TypeOf](using tt, at))

	/** Both operands share a type here (the encoder only emits well-typed nodes), so unlike
	  * `ParsingOps` there is no widening to do — the operation is rebuilt as it was written. */
	private def numOp(op: String, l: Prog, r: Prog): Prog =
		if l.`type`.name != r.`type`.name then
			throw new Exception(s"Cannot apply $op to types ${l.`type`.name} and ${r.`type`.name}")
		else (l.`type`, op) match
			case (Type.IntType, "add")  => (l.unsafe[Int] + r.unsafe[Int]).prog
			case (Type.IntType, "sub")  => (l.unsafe[Int] - r.unsafe[Int]).prog
			case (Type.IntType, "mul")  => (l.unsafe[Int] * r.unsafe[Int]).prog
			case (Type.IntType, "quot") => l.unsafe[Int].quot(r.unsafe[Int]).prog
			case (Type.IntType, "mod")  => (l.unsafe[Int] % r.unsafe[Int]).prog
			case (Type.DoubleType, "add") => (l.unsafe[Double] + r.unsafe[Double]).prog
			case (Type.DoubleType, "sub") => (l.unsafe[Double] - r.unsafe[Double]).prog
			case (Type.DoubleType, "mul") => (l.unsafe[Double] * r.unsafe[Double]).prog
			case (Type.DoubleType, "div") => (l.unsafe[Double] / r.unsafe[Double]).prog
			case _ => throw new Exception(s"Cannot apply $op to type ${l.`type`.name}")

	private def numOp1(op: String, a: Prog): Prog =
		(a.`type`, op) match
			case (Type.IntType, "neg") => a.unsafe[Int].neg.prog
			case (Type.IntType, "abs") => a.unsafe[Int].abs.prog
			case (Type.DoubleType, "neg") => a.unsafe[Double].neg.prog
			case (Type.DoubleType, "abs") => a.unsafe[Double].abs.prog
			case _ => throw new Exception(s"Cannot apply $op to type ${a.`type`.name}")

end ProgramCodec
