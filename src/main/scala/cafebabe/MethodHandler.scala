package cafebabe

import ClassFileTypes._

/** A method handler is used to attach attributes to a method. In particular,
 * it can return an associated <code>CodeHandler</code> which can be used to
 * specify a method's body. <code>MethodHandler</code>s should not be created
 * manually but rather obtained directly when adding a method to a
 * <code>ClassFile</code>. */
class MethodHandler private[cafebabe](m: MethodInfo, c: CodeAttributeInfo, cp: ConstantPool, paramTypes: String) {
  private val method: MethodInfo = m
  private var ch : Option[CodeHandler] = None

  def codeHandler : CodeHandler = {
    if(ch.isEmpty) {
      ch = Some(new CodeHandler(c, cp, paramTypes, m.isStatic))
    }
    ch.get
  }

  def setFlags(flags: U2): Unit = {
    if(ch.isDefined) {
      if(m.isStatic != ((flags & Flags.METHOD_ACC_STATIC) != 0)) {
        sys.error("Cannot change the `static` attribute of a method after its CodeHandler has been issued.")
      }
    }
    m.accessFlags = flags
  }

}
