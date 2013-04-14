package net.fwbrasil.sReflection

import scala.tools.scalap.scalax.rules.scalasig._
import scala.collection.mutable.{ HashMap, SynchronizedMap }
import java.lang.reflect.{ Constructor => JConstructor, Method => JMethod, Array => jArray }
import java.lang.reflect.InvocationTargetException
import java.lang.{ Class => JClass }
import SReflection._

class SParameter[C](
        clazz: Class[_],
        methodName: String,
        paramSymbol: MethodSymbol,
        index: Int) extends Serializable {
    val name = paramSymbol.name
    val genericParameters =
        findGenericParameters(paramSymbol)
    val typ: Class[_] = {
        val clazz = typeToJavaClass(paramSymbol.asInstanceOf[SymbolInfoSymbol].infoType)
        if (clazz.getName == "scala.Array" && genericParameters.head.getName == "scala.Byte")
            classOf[Array[Byte]]
        else
            clazz
    }
    def defaultOption = {
        val prefix =
            if (methodName == "<init>")
                "$lessinit$greater"
            else
                methodName
        val methodOption = getMethod(clazz, prefix + "$default$" + (index + 1))
        methodOption.map(c => (obj: Object) => c.invoke(obj)).asInstanceOf[Option[C => Object]]
    }
}