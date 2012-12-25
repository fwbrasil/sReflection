package net.fwbrasil.sReflection

import scala.tools.scalap.scalax.rules.scalasig._
import scala.collection.mutable.{ HashMap, SynchronizedMap }
import java.lang.reflect.{ Constructor => JConstructor, Method => JMethod, Array => jArray }
import java.lang.reflect.InvocationTargetException
import java.lang.{ Class => JClass }

class SField[C](val clazz: Class[C], val getter: SMethod[C], val setterOption: Option[SMethod[C]]) extends Serializable {
	val fieldType =
		getter.returnType
	val genericParameters =
		getter.fieldGenericParameters
	def get(obj: C) =
		getter.invoke(obj.asInstanceOf[Object])
	def set(obj: C, value: Object) =
		setterOption.get.invoke(obj.asInstanceOf[Object], value)
	def canWrite =
		setterOption.nonEmpty
	def name =
		getter.name
	def isVisibleFrom(obj: Any) =
		getter.isVisibleFrom(obj)
	override def toString =
		clazz.getSimpleName + "." + getter.name
	override def hashCode =
		name.hashCode
	override def equals(other: Any) =
		other != null && classOf[SField[C]].isAssignableFrom(other.getClass) && equals(other.asInstanceOf[SField[C]])
	private def equals(other: SField[C]) =
		other.name == this.name
}