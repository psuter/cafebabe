package cafebabe

import ClassFileTypes._

case class InterfaceInfo(interfaceName: String, nameIndex: U2) extends Streamable {
  override def toStream(stream: ByteStream): ByteStream = {
    stream << nameIndex
  }
}
