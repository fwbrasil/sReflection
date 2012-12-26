package net.fwbrasil.sReflection

import scala.tools.scalap.scalax.rules.scalasig._
import scala.collection.mutable.{ HashMap, SynchronizedMap }
import java.lang.reflect.{ Constructor => JConstructor, Method => JMethod, Array => jArray }
import java.lang.reflect.InvocationTargetException
import java.lang.{ Class => JClass }
import ScalaSigReader._
import SReflection._

class SClass[C](val clazz: Class[C], symbol: ClassSymbol) extends SymbolVisibility(symbol) {
	private val methodSymbols =
		symbol.children.toList collect {
			case m: MethodSymbol => m
		}
	private def collect[T](f: (SClass[C]) => List[T])(f1: => List[T]) = {
			def isScalaClass(clazz: JClass[_]) =
				clazz != null && findScalaSig(clazz).isDefined
		var res = List[T]()
		res ++= f1
		if (isScalaClass(clazz.getSuperclass))
			res ++= f(SClass[C](clazz.getSuperclass.asInstanceOf[JClass[C]]))
		for (int <- clazz.getInterfaces; if (isScalaClass(int)))
			res ++= f(SClass[C](int.asInstanceOf[JClass[C]]))
		res.distinct
	}
	val sConstructors: List[SConstructor[C]] =
		collect(_.sConstructors) {
			if (symbol.isTrait)
				List()
			else
				methodSymbols.filter(isConstructor).map(new SConstructor(clazz, _))
		}
	private val fields =
		methodSymbols.filter(m => !isInit(m) && m.isLocal)
	private val accessors: List[MethodSymbol] =
		methodSymbols.filter(m => !isInit(m) && m.isAccessor)
	private val accessorsNames =
		accessors.map(_.name)
	private def isMethod(m: Symbol) = {
		m.isMethod && !isInit(m) && !m.isLocal && !m.isPrivate && !accessorsNames.contains(m.name.trim) && !m.name.contains("$default$")
	}
	val sMethods: List[SMethod[C]] =
		collect(_.sMethods) {
			methodSymbols.filter(isMethod).map(new SMethod(clazz, _))
		}
	val companionOption = getCompanionObject(clazz).asInstanceOf[Option[C]]
	val sCompanionMethods: List[SMethod[C]] = {
		if (companionOption.isDefined) {
			val scalaSig = findScalaSig(clazz).get
			val companionSymbolOption = (scalaSig.topLevelObjects collect { case o: ObjectSymbol if o.name == clazz.getSimpleName => o }).headOption
			if (companionSymbolOption.isDefined)
				companionSymbolOption.get.infoType.asInstanceOf[TypeRefType].symbol.children.toList.filter(isMethod) collect {
					case symbol: MethodSymbol =>
						new SMethod[C](companionOption.get.getClass.asInstanceOf[Class[C]], symbol)
				}
			else
				List()
		} else
			List()
	}
	val sFields: List[SField[C]] =
		collect(_.sFields) {
			for (getter <- accessors; if (!getter.isPrivate && !getter.name.endsWith("_$eq")))
				yield new SField(clazz, new SMethod(clazz, getter), methodSymbols.find(_.name == getter.name + "_$eq").map(new SMethod(clazz, _)))
		}
	def sBehaviors =
		sConstructors ++ sMethods

	def isInit(methodSymbol: Symbol) =
		methodSymbol.name.trim == "<init>" || methodSymbol.name.trim == "$init$"
	def isConstructor(methodSymbol: Symbol) =
		methodSymbol.name.trim == "<init>"
	private def erasureOf[T: Manifest] =
		manifest[T].runtimeClass.asInstanceOf[Class[T]]
}

object SClass {
	val cache = new HashMap[Class[_], SClass[_]]() with SynchronizedMap[Class[_], SClass[_]]
	def apply[C](clazz: Class[C]) = cache.getOrElseUpdate(clazz, new SClass(clazz, findClass(clazz))).asInstanceOf[SClass[C]]
}