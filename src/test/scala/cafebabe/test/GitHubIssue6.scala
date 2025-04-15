package cafebabe.test

import cafebabe.CodeFreezingException
import cafebabe.ByteCodes._
import cafebabe.AbstractByteCodes._

import org.scalatest.funsuite.AnyFunSuite

class GitHubIssue6 extends AnyFunSuite {

  test("Correct stack height") {
    val cf = new cafebabe.ClassFile("Test", None)

    cf.addDefaultConstructor

    val ch = cf.addMainMethod.codeHandler

    ch <<
      GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<
      Ldc("Hello world!") <<
      InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V") <<
      RETURN

    ch.freeze
  }

  test("Incorrect stack height") {
    val cf = new cafebabe.ClassFile("Test", None)

    cf.addDefaultConstructor

    val ch = cf.addMainMethod.codeHandler

    ch <<
      GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<
      Ldc("Hello world!") <<
      Label("Useless") <<
      InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
    // Mind the missing RETURN.

    intercept[CodeFreezingException] {
      ch.freeze
    }
  }
}
