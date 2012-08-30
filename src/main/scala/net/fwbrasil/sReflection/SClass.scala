package net.fwbrasil.sReflection

import scala.tools.scalap.scalax.rules.scalasig._
import scala.collection.mutable.{ HashMap, SynchronizedMap }
import java.lang.reflect.{ Constructor => JConstructor, Method => JMethod, Array => jArray }
import java.lang.reflect.InvocationTargetException
import java.lang.{ Class => JClass }
import com.sun.tools.example.debug.bdi.MethodNotFoundException
import scala.reflect.Code
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

	def sField(code: Code[(C) => Unit]) =
		try {
			//heuristic
			val method = code.tree.asInstanceOf[scala.reflect.Function].body.asInstanceOf[scala.reflect.Block].stats.head.asInstanceOf[scala.reflect.Select].sym.asInstanceOf[scala.reflect.Method]
			sFields.find(_.name == method.name)
		} catch {
			case e: ClassCastException =>
				throw new IllegalStateException("Can't reflect field", e)
		}

	def sMethod(code: Code[(C) => Unit]): Option[SMethod[C]] =
		sMethod(code)

	def sMethod[V1: Manifest](code: Code[(C, V1) => Unit]): Option[SMethod[C]] =
		sMethod(code, erasureOf[V1])

	def sMethod[V1: Manifest, V2: Manifest](code: Code[(C, V1, V2) => Unit]): Option[SMethod[C]] =
		sMethod(code, erasureOf[V1], erasureOf[V2])

	def sMethod[V1: Manifest, V2: Manifest, V3: Manifest](code: Code[(C, V1, V2, V3) => Unit]): Option[SMethod[C]] =
		sMethod(code, erasureOf[V1], erasureOf[V2], erasureOf[V3])

	def sMethod[V1: Manifest, V2: Manifest, V3: Manifest, V4: Manifest](code: Code[(C, V1, V2, V3, V4) => Unit]): Option[SMethod[C]] =
		sMethod(code, erasureOf[V1], erasureOf[V2], erasureOf[V3], erasureOf[V4])

	def sMethod[V1: Manifest, V2: Manifest, V3: Manifest, V4: Manifest, V5: Manifest](code: Code[(C, V1, V2, V3, V4, V5) => Unit]): Option[SMethod[C]] =
		sMethod(code, erasureOf[V1], erasureOf[V2], erasureOf[V3], erasureOf[V4], erasureOf[V5])

	// heuristic
	private def sMethod(code: Code[_], types: Class[_]*): Option[SMethod[C]] = {
		val method =
			try {
				code.tree.asInstanceOf[scala.reflect.Function].body.asInstanceOf[scala.reflect.Select].sym.asInstanceOf[scala.reflect.Method]
			} catch {
				case e: ClassCastException =>
					try
						code.tree.asInstanceOf[scala.reflect.Function].body.asInstanceOf[scala.reflect.Apply].fun.asInstanceOf[scala.reflect.Select].sym.asInstanceOf[scala.reflect.Method]
					catch {
						case e: ClassCastException =>
							try
								code.tree.asInstanceOf[scala.reflect.Function].body.asInstanceOf[scala.reflect.Block].stats.head.asInstanceOf[scala.reflect.Apply].fun.asInstanceOf[scala.reflect.Select].sym
							catch {
								case e =>
									throw new IllegalStateException("Can't reflect method", e)
							}
					}
			}

			def supers(classes: Set[Class[_]]) =
				classes.map(clazz => clazz.getInterfaces.toSet ++ Option(clazz.getSuperclass)).flatten

			def depthInTree(reference: Class[_], clazz: Class[_]) = {
				if (reference.isAssignableFrom(clazz)) {
					var currSupers = Set[Class[_]](clazz)
					var i = 0
					while (!currSupers.contains(reference)) {
						currSupers = supers(currSupers)
						i += 1
					}
					i
				} else
					throw new IllegalArgumentException("Invalid hierarchy")
			}

		val candidates =
			sMethods.filter(m => {
				val p1 = m.parametersTypes
				val p2 = types.toList
				m.name == method.name && p1.size == p2.size &&
					(0 until p1.size).toList.forall(i => p1(i).isAssignableFrom(p2(i)))
			})
		val sorted = candidates.sortBy {
			m =>
				val dists = (0 until m.parametersTypes.size).toList.map(i => depthInTree(m.parametersTypes(i), types(i)))
				dists.sum
		}
		sorted.headOption
	}
	def isInit(methodSymbol: Symbol) =
		methodSymbol.name.trim == "<init>" || methodSymbol.name.trim == "$init$"
	def isConstructor(methodSymbol: Symbol) =
		methodSymbol.name.trim == "<init>"
	private def erasureOf[T: Manifest] =
		manifest[T].erasure.asInstanceOf[Class[T]]
}

object SClass {
	val cache = new HashMap[Class[_], SClass[_]]() with SynchronizedMap[Class[_], SClass[_]]
	def apply[C](clazz: Class[C]) = cache.getOrElseUpdate(clazz, new SClass(clazz, findClass(clazz))).asInstanceOf[SClass[C]]
}