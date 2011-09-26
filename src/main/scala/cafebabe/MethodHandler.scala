package cafebabe

import ClassFileTypes._

class MethodHandler(m: MethodInfo, c: CodeAttributeInfo, cp: ConstantPool, paramCount: Int) {
  private val method: MethodInfo = m
  private val ch: CodeHandler = new CodeHandler(c,cp,paramCount)
  
  def codeHandler = ch
  
  def setFlags(flags: U2): Unit = { m.accessFlags = flags }
}
