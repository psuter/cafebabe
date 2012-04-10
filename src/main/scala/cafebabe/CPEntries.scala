package cafebabe

import ClassFileTypes._

object CPTags {
  import ClassFileTypes._

  val Class: U1              = 7
  val Fieldref: U1           = 9
  val Methodref: U1          = 10
  val InterfaceMethodref: U1 = 11
  val String: U1             = 8
  val Integer: U1            = 3
  val Float: U1              = 4
  val Long: U1               = 5
  val Double: U1             = 6
  val NameAndType: U1        = 12
  val Utf8: U1               = 1
}

sealed abstract class CPEntry(val tag: U1) extends Streamable

case class CPClassInfo(val nameIndex: U2) extends CPEntry(CPTags.Class) {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << tag << nameIndex
  }
}

case class CPFieldRefInfo(val classIndex: U2, nameAndTypeIndex: U2) extends CPEntry(CPTags.Fieldref) {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << tag << classIndex << nameAndTypeIndex
  }
}

case class CPMethodRefInfo(val classIndex: U2, nameAndTypeIndex: U2) extends CPEntry(CPTags.Methodref) {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << tag << classIndex << nameAndTypeIndex
  }
}

case class CPInterfaceMethodRefInfo(val classIndex: U2, nameAndTypeIndex: U2) extends CPEntry(CPTags.InterfaceMethodref) {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << tag << classIndex << nameAndTypeIndex
  }
}

case class CPStringInfo(val stringIndex: U2) extends CPEntry(CPTags.String) {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << tag << stringIndex
  }
}

case class CPIntegerInfo(val bytes: U4) extends CPEntry(CPTags.Integer) {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << tag << bytes
  }
}

case class CPFloatInfo(val bytes: U4) extends CPEntry(CPTags.Float) {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << tag << bytes
  }
}

case class CPLongInfo(val highBytes: U4, val lowBytes: U4) extends CPEntry(CPTags.Long) {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << tag << highBytes << lowBytes
  }
}

case class CPDoubleInfo(val highBytes: U4, val lowBytes: U4) extends CPEntry(CPTags.Double) {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << tag << highBytes << lowBytes
  }
}

case class CPNameAndTypeInfo(val nameIndex: U2, val descriptorIndex: U2) extends CPEntry(CPTags.NameAndType) {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << tag << nameIndex << descriptorIndex
  }
}

case class CPUtf8Info(val bytes: Seq[U1]) extends CPEntry(CPTags.Utf8) {
  private var original: String = _

  def setSource(str: String): CPUtf8Info = { original = str; this }
  def getSource: String = original

  override def toStream(stream: ByteStream): ByteStream = {
    stream << tag << bytes.length.asInstanceOf[U2]
    bytes.foreach(b => { stream << b })
    stream
  }
}
