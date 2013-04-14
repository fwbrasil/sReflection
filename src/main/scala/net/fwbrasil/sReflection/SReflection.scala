package net.fwbrasil.sReflection

import language.implicitConversions
import language.reflectiveCalls
import scala.tools.scalap.scalax.rules.scalasig._
import scala.collection.mutable.{ HashMap, SynchronizedMap }
import java.lang.reflect.{ Constructor => JConstructor, Method => JMethod, Array => jArray }
import java.lang.reflect.InvocationTargetException
import java.lang.{ Class => JClass }

object SReflection {

    import ScalaSigReader._

    implicit def getSClass[C](obj: C) = new { def getSClass = SClass(obj.getClass.asInstanceOf[Class[C]]) }
    implicit def toSClass[C](clazz: Class[C]): SClass[C] = SClass(clazz)

    def sClassOf[C: Manifest] = SClass(manifest[C].runtimeClass).asInstanceOf[SClass[C]]

    def findGenericParameters(typeRefType: TypeRefType): List[Class[_]] =
        typeRefType.typeArgs.map(typeToJavaClass(_)).toList

    def findGenericParameters(symbol: MethodSymbol): List[Class[_]] =
        findGenericParameters(symbol.infoType)
    def findGenericParameters(typ: Type): List[Class[_]] =
        typ match {
            case typeRefType: TypeRefType =>
                findGenericParameters(typeRefType)
            case _ =>
                List()
        }

    def findParameters[C](clazz: Class[_], s: MethodSymbol): List[SParameter[C]] = {
        val children = s.children
        (for (i <- 0 until children.length; if (children(i).isInstanceOf[MethodSymbol])) yield {
            new SParameter[C](clazz, s.name, children(i).asInstanceOf[MethodSymbol], i)
        }).toList
    }

    def findArgTypeForField(s: MethodSymbol, typeArgIdx: Int): Class[_] = {
        val t = s.infoType.asInstanceOf[{ def resultType: Type }].resultType match {
            case TypeRefType(_, _, args) => args(typeArgIdx)
        }

        def findPrimitive(t: Type): Symbol = t match {
            case TypeRefType(ThisType(_), symbol, _) => symbol
            case ref @ TypeRefType(_, _, _) => findPrimitive(ref)
            case x => fail("Unexpected type info " + x)
        }
        typeToJavaClass(t)
    }
    import scala.List

    def classToJava(clazz: Symbol): JClass[Any] =
        try
            toSimpleScalaType(clazz).getOrElse(packageObjectClass(clazz).getOrElse(Class.forName(clazz.path).asInstanceOf[Class[Any]]))
        catch {
            case e: ClassNotFoundException =>
                classOf[Any]
        }

    def isModule(clazz: Symbol) =
        try {
            Class.forName(clazz.path + "$")
            true
        } catch {
            case e: ClassNotFoundException => false
        }

    def packageObjectClass(clazz: Symbol) =
        clazz.parent.filter(isModule).map { parent =>
            val packageClass = Class.forName(parent.path + "$")
            val packObject = packageClass.getField("MODULE$").get(packageClass)
            val methodOption = try
                Some(packageClass.getMethod(clazz.name))
            catch {
                case e: NoSuchMethodException => None
            }
            if (methodOption.isDefined) {
                val method = methodOption.get
                val classObject = method.invoke(packObject)
                val classObjectName = classObject.getClass.getName
                val className = classObjectName.substring(0, classObjectName.size - 1)
                Class.forName(className).asInstanceOf[JClass[Any]]
            } else
                classOf[Any]
        }

    def typeToJavaClass(tpe: Type): JClass[_] = tpe match {
        case ExistentialType(rtpe, _) => typeToJavaClass(rtpe)
        case TypeRefType(_, sym, _) => classToJava(sym)
        case _ => classOf[Object] //throw new NoClassDefFoundError("no Java class corresponding to " + tpe + " found")
    }

    def toSimpleScalaType(s: Symbol) =
        Option((s.path match {
            case "scala.Short" => classOf[Short]
            case "scala.Char" => classOf[Char]
            case "scala.Int" => classOf[Int]
            case "scala.Long" => classOf[Long]
            case "scala.Boolean" => classOf[Boolean]
            case "scala.Float" => classOf[Float]
            case "scala.Double" => classOf[Double]
            case "scala.Predef.String" => classOf[String]
            case "scala.Any" => classOf[Object]
            case "scala.Nothing" => classOf[Unit]
            case "scala.<byname>" => classOf[Function0[_]]
            case "scala.<repeated>" => classOf[Array[Object]]
            case "scala.Predef.Class" => classOf[Class[_]]
            case _ => null
        }).asInstanceOf[Class[Any]])

    def getCompanionObject(clazz: Class[_]) =
        _getCompanionObject(clazz)
            .orElse(_getCompanionObject(Class.forName(clazz.getName + "$")))

    private def _getCompanionObject(companionClass: => Class[_]) =
        try {
            val field = companionClass.getField("MODULE$")
            Option(field.get(companionClass))
        } catch {
            case e: ClassNotFoundException =>
                None
            case e: NoSuchFieldException =>
                None
        }

    def getMethod(clazz: Class[_], name: String, paramsTypes: Class[_]*) =
        try
            Option(clazz.getMethod(name, paramsTypes: _*))
        catch {
            case e: NoSuchMethodException =>
                None
        }

}
