package net.fwbrasil.sReflection

import scala.tools.scalap.scalax.rules.scalasig.SymbolInfo

abstract class SymbolVisibility(symbol: {
	def symbolInfo: SymbolInfo
	def isProtected: Boolean
	def isPrivate: Boolean
}) extends Serializable {
	def clazz: Class[_]
	val privateWithin =
		symbol.symbolInfo.privateWithin.map(_.toString)
	val isProtected =
		symbol.isProtected
	val isPrivate =
		symbol.isPrivate
	def isVisibleFrom(obj: Any) = {
		val source = obj.getClass
		val sourcePackage = source.getPackage.getName
		if (privateWithin.isDefined) {
			val targetPackage = privateWithin.get
			sourcePackage.startsWith(targetPackage)
		} else if (isProtected) {
			clazz.isAssignableFrom(source)
		} else if (isPrivate) {
			clazz == source
		} else
			true
	}
}