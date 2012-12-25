package net.fwbrasil.sReflection

import scala.tools.scalap.scalax.rules.scalasig._
import scala.collection.mutable.{ HashMap, SynchronizedMap }
import java.lang.reflect.{ Constructor => JConstructor, Method => JMethod, Array => jArray }
import java.lang.reflect.InvocationTargetException
import java.lang.{ Class => JClass }
import scala.reflect.Code
import ScalaSigReader._
import SReflection._

abstract class SBehavior[C](symbol: MethodSymbol, val name: String) extends SymbolVisibility(symbol) {
	val clazz: Class[_]
	val companionOption = getCompanionObject(clazz)
	def jBehavior: JType
	val returnType: Class[_]
	type JType <: {
		def getParameterTypes(): Array[Class[_]]
		def getName(): String
	}
	val parameters =
		findParameters[C](clazz, symbol).toList
	def parametersTypes =
		parameters.map(_.typ)
	def parametersNames =
		parameters.map(_.name)
	def parametersDefaults =
		parameters.map(_.defaultOption)
	val isAccessor =
		symbol.isAccessor
	val isGetter =
		symbol.isAccessor && !name.endsWith("_$eq")
	val isSetter =
		symbol.isAccessor && !isGetter
	def invoke(args: Object*): Object
	override def toString =
		clazz.getSimpleName + "." + name + "(" + parameters.map(c => c.name + ": " + c.typ.getName + (if (!c.defaultOption.isDefined) "" else " = " + c.defaultOption)).mkString(", ") + ")"
	override def hashCode =
		name.hashCode + parameters.hashCode
	override def equals(other: Any) =
		other != null && classOf[SBehavior[C]].isAssignableFrom(other.getClass) && equals(other.asInstanceOf[SBehavior[_]])
	private def equals(other: SBehavior[_]) =
		other.name == this.name && other.parametersTypes == this.parametersTypes
}
