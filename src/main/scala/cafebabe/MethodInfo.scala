package cafebabe

import ClassFileTypes._

case class MethodInfo(var accessFlags: U2, nameIndex: U2, descriptorIndex: U2, attributes: Seq[AttributeInfo]) extends Streamable {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << accessFlags << nameIndex << descriptorIndex
    stream << attributes.size.asInstanceOf[U2] << attributes
  }

  def isStatic : Boolean = (accessFlags & Flags.METHOD_ACC_STATIC) != 0
}
