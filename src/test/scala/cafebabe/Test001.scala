package cafebabe
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Test001 extends FunSuite with ShouldMatchers {
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


    //cf.dynamicallyLoad

    val byteStream = (new ByteStream) << cf
    val bytes : Array[Byte] = byteStream.getBytes
    CustomClassLoader.registerClass("Hello", bytes)

    val clazz = CustomClassLoader.loadClass("Hello")

    val defaultCons = clazz.getConstructor()
    defaultCons.setAccessible(true)
    val inst = defaultCons.newInstance()

    val meth = clazz.getMethod("hw")
    meth.setAccessible(true)
    val result : String = meth.invoke(inst).asInstanceOf[String]
    result should equal("Hello world!")
  }


}
