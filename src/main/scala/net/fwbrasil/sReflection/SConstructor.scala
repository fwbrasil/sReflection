package net.fwbrasil.sReflection

import scala.tools.scalap.scalax.rules.scalasig._
import scala.collection.mutable.{ HashMap, SynchronizedMap }
import java.lang.reflect.{ Constructor => JConstructor, Method => JMethod, Array => jArray }
import java.lang.reflect.InvocationTargetException
import java.lang.{ Class => JClass }
import java.lang.reflect.Constructor

class SConstructor[C](val clazz: Class[C], symbol: MethodSymbol) extends SBehavior[C](symbol, "novo") {
    type JType = JConstructor[C]
    def jBehavior: Constructor[C] =
        try
            clazz.getConstructor(parametersTypes: _*)
        catch {
            case e: NoSuchMethodException =>
                if (clazz.getDeclaringClass != null)
                    clazz.getConstructor(List(clazz.getDeclaringClass).++(parametersTypes): _*)
                else null
        }
    val returnType =
        clazz
    def invoke(args: Object*) =
        jBehavior.newInstance(args: _*).asInstanceOf[Object]
}