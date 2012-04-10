package cafebabe

import ClassFileTypes._

object CodeAttributeInfo {
  def apply(codeNameIndex: U2) : CodeAttributeInfo =
    new CodeAttributeInfo(codeNameIndex)

  def unapply(cai: CodeAttributeInfo) : Option[U2] =
    if(cai == null) None else Some(cai.codeNameIndex)
}

class CodeAttributeInfo(val codeNameIndex: U2) extends AttributeInfo(codeNameIndex, Nil) {
  var maxStack: U2 = 0  // gets set when the code handler 'freezes'
  var maxLocals: U2 = 0 // gets set when the code handler 'freezes'
  var code: ByteStream = new ByteStream

  case class ExceptionTableEntry(startPC: U2, endPC: U2, handlerPC: U2, catchType: U2) extends Streamable {
    override def toStream(stream: ByteStream) = stream
  }
  var exceptionTable: Seq[ExceptionTableEntry] = Nil
  var attributes: Seq[AttributeInfo] = Nil

  override def toStream(stream: ByteStream): ByteStream = {
    val codeLength: U4 = code.size.asInstanceOf[U4]
    val exceptionTableLength: U2 = exceptionTable.size.asInstanceOf[U2]
    val attributesCount: U2 = attributes.size.asInstanceOf[U2]

    val totalLength = size
    stream << codeNameIndex << (totalLength-6).asInstanceOf[U4] << maxStack << maxLocals << codeLength << code
    stream << exceptionTableLength << exceptionTable
    stream << attributesCount << attributes
  }

  private def attributesSize: Int = {
    attributes.foldLeft[Int](0)((s:Int, c:AttributeInfo) => { s + c.size })
  }

  override def size: Int = {
    18 + code.size + (exceptionTable.size * 8) + attributesSize
  }
}

