package net.fwbrasil.sReflection

import org.junit.runner._
import org.specs2.mutable._
import org.specs2.runner._

class Class1 {
	def method1 = {}
	def method1(s: String) = s
	def method1(i: Int) = i
	def method1(c: Class[_]) = c
	def method1(a: Any) = a
	def method2() = {}
	def method2(a: Any, b: String) = {}
	def method2(a: String, b: Any) = {}
}

import SReflection._

@RunWith(classOf[JUnitRunner])
class SMethodSpecs extends Specification {

	val clazz = classOf[Class1]
	val instance = new Class1

	"SReflection" should {

		"reflect method without parameter parentheses" in {
			val m = clazz.sMethod(_.method1).get
			m.parameters must beEmpty
			m.name mustEqual "method1"
			m.invoke(instance) must beNull // Unit is returned as null
		}

		"reflect method with empty parameter parentheses" in {
			val m = clazz.sMethod(_.method2()).get
			m.parameters must beEmpty
			m.name mustEqual "method2"
			m.invoke(instance) must beNull
			m.invoke(instance, "a") must throwA[IllegalArgumentException]
		}

		"reflect method without parameter parentheses 2" in {
			val m = clazz.sMethod(_.method2).get
			m.parameters must beEmpty
			m.name mustEqual "method2"
			m.invoke(instance) must beNull
			m.invoke(instance, "a") must throwA[IllegalArgumentException]
		}

		"reflect method with string parameter" in {
			val m = clazz.sMethod[String](_.method1(_)).get
			m.parameters.map(_.typ) mustEqual List(classOf[String])
			m.parameters.map(_.name) mustEqual List("s")
			m.name mustEqual "method1"
			m.invoke(instance) must throwA[IllegalArgumentException]
			m.invoke(instance, "a") mustEqual "a"
		}

		"reflect method with int parameter" in {
			val m = clazz.sMethod[Int](_.method1(_)).get
			m.parameters.map(_.typ) mustEqual List(classOf[Int])
			m.parameters.map(_.name) mustEqual List("i")
			m.name mustEqual "method1"
			m.invoke(instance) must throwA[IllegalArgumentException]
			m.invoke(instance, 1: java.lang.Integer) mustEqual 1
		}

		"reflect method with class parameter" in {
			val m = clazz.sMethod[Class[_]](_.method1(_)).get
			m.parameters.map(_.typ) mustEqual List(classOf[Class[_]])
			m.parameters.map(_.name) mustEqual List("c")
			m.name mustEqual "method1"
			m.invoke(instance) must throwA[IllegalArgumentException]
			m.invoke(instance, clazz) mustEqual clazz
		}
	}

}