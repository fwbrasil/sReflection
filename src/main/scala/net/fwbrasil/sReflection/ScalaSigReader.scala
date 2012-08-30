package net.fwbrasil.sReflection

import scala.tools.scalap.scalax.rules.scalasig.ScalaSig
import scala.tools.scalap.scalax.rules.scalasig.ClassSymbol
import scala.collection.mutable.HashMap
import scala.collection.mutable.SynchronizedMap
import scala.tools.scalap.scalax.rules.scalasig.ScalaSigParser
import scala.tools.scalap.scalax.rules.scalasig.TypeRefType

object ScalaSigReader {
	def fail(msg: String) = throw new IllegalStateException(msg)

	@transient
	private var cache: HashMap[Class[_], Option[ScalaSig]] = null

	def findScalaSig(clazz: Class[_]): Option[ScalaSig] =
		if (clazz == null)
			None
		else {
			if (cache == null)
				cache = new HashMap[Class[_], Option[ScalaSig]]() with SynchronizedMap[Class[_], Option[ScalaSig]]
			cache.getOrElseUpdate(clazz, ScalaSigParser.parse(clazz).orElse(findScalaSig(clazz.getDeclaringClass)))
		}

	def findClass(clazz: Class[_]): ClassSymbol = {
		val sig = findScalaSig(clazz).getOrElse(fail("Can't find ScalaSig for " + clazz))
		findClass(sig, clazz).getOrElse(fail("Can't find " + clazz + " from parsed ScalaSig"))
	}

	private def findClass(sig: ScalaSig, clazz: Class[_]): Option[ClassSymbol] = {
		sig.symbols.collect { case c: ClassSymbol if !c.isModule => c }.find(_.name == clazz.getSimpleName).orElse {
			sig.topLevelClasses.find(_.symbolInfo.name == clazz.getSimpleName).orElse {
				sig.topLevelObjects.map { obj =>
					val t = obj.infoType.asInstanceOf[TypeRefType]
					t.symbol.children collect { case c: ClassSymbol => c } find (_.symbolInfo.name == clazz.getSimpleName)
				}.head
			}
		}
	}
}
