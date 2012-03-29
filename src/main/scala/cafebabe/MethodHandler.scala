package cafebabe

import ClassFileTypes._

/** A method handler is used to attach attributes to a method. In particular,
 * it can return an associated <code>CodeHandler</code> which can be used to
 * specify a method's body. <code>MethodHandler</code>s should not be created
 * manually but rather obtained directly when adding a method to a
 * <code>ClassFile</code>. */
class MethodHandler private[cafebabe](m: MethodInfo, c: CodeAttributeInfo, cp: ConstantPool, paramCount: Int) {
  private val method: MethodInfo = m
  private val ch: CodeHandler = new CodeHandler(c,cp,paramCount)
  
  def codeHandler = ch
  
  def setFlags(flags: U2): Unit = { m.accessFlags = flags }
}
