package io.vernix

import Prog.prog

object ParsingOps:
	def op_+(l: Prog, r: Prog): Prog =
		(l.`type`, r.`type`) match
			case (Type.IntType, Type.IntType) => (l.unsafe[Int] + r.unsafe[Int]).prog
			case (Type.DoubleType, Type.DoubleType) => (l.unsafe[Double] + r.unsafe[Double]).prog
			case (Type.IntType, Type.DoubleType) => (l.unsafe[Int].toDouble + r.unsafe[Double]).prog
			case (Type.DoubleType, Type.IntType) => (l.unsafe[Double] + r.unsafe[Int].toDouble).prog
			case _ => throw new Exception(s"Cannot add types ${l.`type`.name} and ${r.`type`.name}")

	def op_-(l: Prog, r: Prog): Prog =
		(l.`type`, r.`type`) match
			case (Type.IntType, Type.IntType) => (l.unsafe[Int] - r.unsafe[Int]).prog
			case (Type.DoubleType, Type.DoubleType) => (l.unsafe[Double] - r.unsafe[Double]).prog
			case (Type.IntType, Type.DoubleType) => (l.unsafe[Int].toDouble - r.unsafe[Double]).prog
			case (Type.DoubleType, Type.IntType) => (l.unsafe[Double] - r.unsafe[Int].toDouble).prog
			case _ => throw new Exception(s"Cannot subtract types ${l.`type`.name} and ${r.`type`.name}")

	def op_*(l: Prog, r: Prog): Prog =
		(l.`type`, r.`type`) match
			case (Type.IntType, Type.IntType) => (l.unsafe[Int] * r.unsafe[Int]).prog
			case (Type.DoubleType, Type.DoubleType) => (l.unsafe[Double] * r.unsafe[Double]).prog
			case (Type.IntType, Type.DoubleType) => (l.unsafe[Int].toDouble * r.unsafe[Double]).prog
			case (Type.DoubleType, Type.IntType) => (l.unsafe[Double] * r.unsafe[Int].toDouble).prog
			case _ => throw new Exception(s"Cannot multiply types ${l.`type`.name} and ${r.`type`.name}")

	def op_/(l: Prog, r: Prog): Prog =
		(l.`type`, r.`type`) match
			case (Type.IntType, Type.IntType) => l.unsafe[Int].quot(r.unsafe[Int]).prog
			case (Type.DoubleType, Type.DoubleType) => (l.unsafe[Double] / r.unsafe[Double]).prog
			case (Type.IntType, Type.DoubleType) => (l.unsafe[Int].toDouble / r.unsafe[Double]).prog
			case (Type.DoubleType, Type.IntType) => (l.unsafe[Double] / r.unsafe[Int].toDouble).prog
			case _ => throw new Exception(s"Cannot divide types ${l.`type`.name} and ${r.`type`.name}")

	def op_%(l: Prog, r: Prog): Prog =
		(l.`type`, r.`type`) match
			case (Type.IntType, Type.IntType) => (l.unsafe[Int] % r.unsafe[Int]).prog
			case _ => throw new Exception(s"Cannot compute remainder for types ${l.`type`.name} and ${r.`type`.name}")

	def op_<(l: Prog, r: Prog): Prog.Aux[Boolean] =
		(l.`type`, r.`type`) match
			case (Type.IntType, Type.IntType) => (l.unsafe[Int] < r.unsafe[Int]).prog
			case (Type.DoubleType, Type.DoubleType) => (l.unsafe[Double] < r.unsafe[Double]).prog
			case (Type.IntType, Type.DoubleType) => (l.unsafe[Int].toDouble < r.unsafe[Double]).prog
			case (Type.DoubleType, Type.IntType) => (l.unsafe[Double] < r.unsafe[Int].toDouble).prog
			case (Type.StringType, Type.StringType) => (l.unsafe[String] < r.unsafe[String]).prog
			case _ => throw new Exception(s"Cannot compare types ${l.`type`.name} and ${r.`type`.name}")

	def op_<=(l: Prog, r: Prog): Prog.Aux[Boolean] =
		(l.`type`, r.`type`) match
			case (Type.IntType, Type.IntType) => (l.unsafe[Int] <= r.unsafe[Int]).prog
			case (Type.DoubleType, Type.DoubleType) => (l.unsafe[Double] <= r.unsafe[Double]).prog
			case (Type.IntType, Type.DoubleType) => (l.unsafe[Int].toDouble <= r.unsafe[Double]).prog
			case (Type.DoubleType, Type.IntType) => (l.unsafe[Double] <= r.unsafe[Int].toDouble).prog
			case (Type.StringType, Type.StringType) => (l.unsafe[String] <= r.unsafe[String]).prog
			case _ => throw new Exception(s"Cannot compare types ${l.`type`.name} and ${r.`type`.name}")

	def op_>(l: Prog, r: Prog): Prog.Aux[Boolean] =
		(l.`type`, r.`type`) match
			case (Type.IntType, Type.IntType) => (l.unsafe[Int] > r.unsafe[Int]).prog
			case (Type.DoubleType, Type.DoubleType) => (l.unsafe[Double] > r.unsafe[Double]).prog
			case (Type.IntType, Type.DoubleType) => (l.unsafe[Int].toDouble > r.unsafe[Double]).prog
			case (Type.DoubleType, Type.IntType) => (l.unsafe[Double] > r.unsafe[Int].toDouble).prog
			case (Type.StringType, Type.StringType) => (l.unsafe[String] > r.unsafe[String]).prog
			case _ => throw new Exception(s"Cannot compare types ${l.`type`.name} and ${r.`type`.name}")

	def op_>=(l: Prog, r: Prog): Prog.Aux[Boolean] =
		(l.`type`, r.`type`) match
			case (Type.IntType, Type.IntType) => (l.unsafe[Int] >= r.unsafe[Int]).prog
			case (Type.DoubleType, Type.DoubleType) => (l.unsafe[Double] >= r.unsafe[Double]).prog
			case (Type.IntType, Type.DoubleType) => (l.unsafe[Int].toDouble >= r.unsafe[Double]).prog
			case (Type.DoubleType, Type.IntType) => (l.unsafe[Double] >= r.unsafe[Int].toDouble).prog
			case (Type.StringType, Type.StringType) => (l.unsafe[String] >= r.unsafe[String]).prog
			case _ => throw new Exception(s"Cannot compare types ${l.`type`.name} and ${r.`type`.name}")

	def op_===(l: Prog, r: Prog): Prog.Aux[Boolean] =
		(l.`type`, r.`type`) match
			case (Type.IntType, Type.DoubleType) => (l.unsafe[Int].toDouble === r.unsafe[Double]).prog
			case (Type.DoubleType, Type.IntType) => (l.unsafe[Double] === r.unsafe[Int].toDouble).prog
			case _ if (l.`type`.name == r.`type`.name) =>
				given Type[l.T] = l.`type`
				(l.unsafe[l.T] === r.unsafe[l.T]).prog
			case _ => throw new Exception(s"Cannot compare types ${l.`type`.name} and ${r.`type`.name}")

	def op_!==(l: Prog, r: Prog): Prog.Aux[Boolean] =
		(l.`type`, r.`type`) match
			case (Type.IntType, Type.DoubleType) => (l.unsafe[Int].toDouble !== r.unsafe[Double]).prog
			case (Type.DoubleType, Type.IntType) => (l.unsafe[Double] !== r.unsafe[Int].toDouble).prog
			case _ if (l.`type`.name == r.`type`.name) =>
				given Type[l.T] = l.`type`
				(l.unsafe[l.T] !== r.unsafe[l.T]).prog
			case _ => throw new Exception(s"Cannot compare types ${l.`type`.name} and ${r.`type`.name}")

	def op_&(l: Prog, r: Prog): Prog.Aux[Boolean] =
		(l.`type`, r.`type`) match
			case (Type.BooleanType, Type.BooleanType) => (l.unsafe[Boolean] && r.unsafe[Boolean]).prog
			case _ => throw new Exception(s"Cannot apply & boolean operator on types ${l.`type`.name} and ${r.`type`.name}")

	def op_|(l: Prog, r: Prog): Prog.Aux[Boolean] =
		(l.`type`, r.`type`) match
			case (Type.BooleanType, Type.BooleanType) => (l.unsafe[Boolean] || r.unsafe[Boolean]).prog
			case _ => throw new Exception(s"Cannot apply | boolean operator on types ${l.`type`.name} and ${r.`type`.name}")

	def opIf(cond: Prog, ifTrue: Prog, ifFalse: Prog): Prog =
		cond.`type` match
			case Type.BooleanType =>
				if ifTrue.`type`.name == ifFalse.`type`.name then
					given Type[ifTrue.T] = ifTrue.`type`
					Program.ifElse(cond.unsafe[Boolean])(ifTrue.program, ifFalse.unsafe[ifTrue.T]).prog
				else
					throw new Exception(s"Cannot choose between types ${ifTrue.`type`.name} and ${ifFalse.`type`.name}")
			case _ => throw new Exception(s"Condition must be of type Boolean, not ${cond.`type`.name}")

	def repeatUntil(action: Prog, condition: Prog): Prog =
		condition.`type` match
			case Type.BooleanType => Program.repeatUntil(action.program)(condition.unsafe[Boolean]).prog(using action.`type`)
			case _ => throw new Exception(s"Condition must be of type Boolean, not ${condition.`type`.name}")

	def doWhile(condition: Prog, action: Prog): Prog.Aux[Unit] =
		condition.`type` match
			case Type.BooleanType => Program.whileDo(condition.unsafe[Boolean])(action.program).prog
			case _ => throw new Exception(s"Condition must be of type Boolean, not ${condition.`type`.name}")

end ParsingOps
