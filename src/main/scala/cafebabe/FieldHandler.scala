package cafebabe

import ClassFileTypes._

class FieldHandler(f: FieldInfo, cp: ConstantPool) {
  private val field: FieldInfo = f

  def setFlags(flags : U2) : Unit = { f.accessFlags = flags }
}
