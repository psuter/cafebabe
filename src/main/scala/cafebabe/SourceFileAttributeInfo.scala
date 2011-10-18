package cafebabe

import ClassFileTypes._

case class SourceFileAttributeInfo(override val attributeNameIndex: U2, sourceFileIndex : U2) extends AttributeInfo(attributeNameIndex, Nil) {
  override def toStream(stream : ByteStream) : ByteStream = {
    stream << attributeNameIndex << (2 : U4) << sourceFileIndex
  }
}
