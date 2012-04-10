package cafebabe

import ClassFileTypes._

object AttributeInfo {
  def apply(attributeNameIndex: U2, info: Seq[U1]) : AttributeInfo =
    new AttributeInfo(attributeNameIndex, info)

  def unapply(ai: AttributeInfo) : Option[(U2,Seq[U1])] =
    if(ai == null) None else Some((ai.attributeNameIndex, ai.info))
}

class AttributeInfo(val attributeNameIndex: U2, val info: Seq[U1]) extends Streamable {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << attributeNameIndex
    stream << info.size.asInstanceOf[U4]
    info.foreach(stream << _)
    stream
  }

  def size: Int = 6 + info.size
}

