import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class Test001 extends FunSuite with ShouldMatchers {
  import cafebabe._
  import cafebabe.AbstractByteCodes._
  import cafebabe.ByteCodes._

  val ten = 10

  test("Hello World") {
    val cf = new ClassFile("Hello", None)
    cf.addDefaultConstructor
    val ch = cf.addMethod("Ljava/lang/String;", "hw").codeHandler
    ch << Ldc("Hello world!")
    ch << ARETURN
    ch.freeze

    cf.dynamicallyLoad

    try {
      val clazz = Class.forName("Hello")
      val defaultCons = clazz.getConstructor()
      val inst = defaultCons.newInstance()
      val meth = clazz.getMethod("hw", Class.forName("java.lang.String"))
      val result : String = meth.invoke(inst).asInstanceOf[String]
      result should equal("Hello world!")
    } catch {
      case e => {
        println(e)
        true should equal(false)
      }
    }
  }


}
