package net.fwbrasil.sReflection

import scala.tools.scalap.scalax.rules.scalasig._
import scala.collection.mutable.{ HashMap, SynchronizedMap }
import java.lang.reflect.{ Constructor => JConstructor, Method => JMethod, Array => jArray }
import java.lang.reflect.InvocationTargetException
import java.lang.{ Class => JClass }
import com.sun.tools.example.debug.bdi.MethodNotFoundException
import scala.reflect.Code
import SReflection._

class SParameter[C](
		clazz: Class[_],
		methodName: String,
		paramSymbol: MethodSymbol,
		index: Int) extends Serializable {
	val name = paramSymbol.name
	val typ = typeToJavaClass(paramSymbol.asInstanceOf[SymbolInfoSymbol].infoType)
	def defaultOption = {
		val prefix =
			if (methodName == "<init>")
				"init"
			else
				methodName
		val methodOption = getMethod(clazz, prefix + "$default$" + (index + 1))
		methodOption.map(c => (obj: Object) => c.invoke(obj)).asInstanceOf[Option[C => Object]]
	}
	val genericParameters =
		findGenericParameters(paramSymbol)
}