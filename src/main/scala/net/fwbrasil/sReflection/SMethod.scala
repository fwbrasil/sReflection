package net.fwbrasil.sReflection

import scala.tools.scalap.scalax.rules.scalasig._
import scala.collection.mutable.{ HashMap, SynchronizedMap }
import java.lang.reflect.{ Constructor => JConstructor, Method => JMethod, Array => jArray }
import java.lang.reflect.InvocationTargetException
import java.lang.{ Class => JClass }
import SReflection._

class SMethod[C](val clazz: Class[C], val symbol: MethodSymbol) extends SBehavior[C](symbol, symbol.name.trim) {
	type JType = JMethod
	def jBehavior =
		try {
			clazz.getDeclaredMethod(name, parametersTypes: _*)
		} catch {
			case e: NoSuchMethodException =>
				clazz.getDeclaredMethods.find(
					c => c.getName == name && c.getParameterTypes.size == parametersTypes.size).getOrElse(throw e)
		}
	val returnType =
		jBehavior.getReturnType
	val fieldGenericParameters =
		if (symbol.isAccessor && symbol.infoType.isInstanceOf[NullaryMethodType])
			findGenericParameters(
				symbol.infoType.asInstanceOf[NullaryMethodType].resultType)
		else List()
	def invoke(args: Object*) =
		if (args.isEmpty)
			throw new IllegalArgumentException("First parameter must be the receiver")
		else
			jBehavior.invoke(args(0), args.tail: _*)
}