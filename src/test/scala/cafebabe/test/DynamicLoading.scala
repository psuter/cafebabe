package cafebabe.test

import cafebabe._
import cafebabe.ByteCodes._
import cafebabe.AbstractByteCodes._

import org.scalatest.FunSuite
class DynamicLoading extends FunSuite {
  test("DL 1") {
    val cf = new ClassFile("MyTest", None)
    cf.addDefaultConstructor
    val ch = cf.addMethod("I", "plusOne", "I").codeHandler
    ch << ILoad(1) << Ldc(1) << IADD << IRETURN
    ch.freeze

    cf.dynamicallyLoad
    
    val c = CustomClassLoader.loadClass("MyTest")
    val o = c.newInstance().asInstanceOf[AnyRef]
    val m = c.getMethod("plusOne", Integer.TYPE)
    (m.invoke(o, 41 : java.lang.Integer) === 42)
  }


}
