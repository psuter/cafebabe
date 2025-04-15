package cafebabe.test

import cafebabe._
import cafebabe.ByteCodes._
import cafebabe.AbstractByteCodes._

import org.scalatest.funsuite.AnyFunSuite

class DynamicLoading extends AnyFunSuite {
  private def mkMinimalClassFile : ClassFile = {
    val cf = new ClassFile("MyTest", None)
    cf.addDefaultConstructor
    val ch = cf.addMethod("I", "plusOne", "I").codeHandler
    ch << ILoad(1) << Ldc(1) << IADD << IRETURN
    ch.freeze
    cf
  }

  test("DL 1") {
    val cf = mkMinimalClassFile

    val cl = new CafebabeClassLoader
    cl.register(cf)
    
    val c = cl.loadClass("MyTest")
    val o = c.getDeclaredConstructor().newInstance().asInstanceOf[AnyRef]
    val m = c.getMethod("plusOne", Integer.TYPE)

    (m.invoke(o, 41 : java.lang.Integer) === 42)
  }

  test("DL 2") {
    // NB this test is incomplete.
    val cf = mkMinimalClassFile

    val cl = new CafebabeClassLoader
    cl.register(cf)
    val dynObj = cl.newInstance("MyTest")
    // dynObj.plusOne(41) would ideally work...
  }
}
