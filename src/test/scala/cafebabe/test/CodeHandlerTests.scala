package cafebabe.test

import cafebabe._
import cafebabe.ByteCodes._
import cafebabe.AbstractByteCodes._

import org.scalatest.FunSuite

class CodeHandlerTests extends FunSuite {
  private def mkCodeHandler() : CodeHandler = {
    val cf = new cafebabe.ClassFile("Test", None)
    cf.addDefaultConstructor
    cf.addMainMethod.codeHandler
  }

  test("Cannot freeze twice") {
    val ch = mkCodeHandler()
    ch << RETURN
    ch.freeze

    intercept[CodeFreezingException] {
      ch.freeze
    }
  }
}
