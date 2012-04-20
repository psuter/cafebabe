package cafebabe

trait Streamable {
  def toStream(byteStream: ByteStream): ByteStream
}

/** Wrapper around various Java streams. */
class ByteStream {
  import java.io.{DataOutputStream,ByteArrayOutputStream}
  import ClassFileTypes._

  private var bytes = new ByteArrayOutputStream
  def getBytes : Array[Byte] = { stream.flush() ; bytes.toByteArray }
  private var stream: DataOutputStream = new DataOutputStream(bytes)

  // appends bytes to the stream
  def <<(u1: U1): ByteStream = { stream.write(u1); this }
  def <<(u2: U2): ByteStream = { stream.writeShort(u2); this }
  def <<(u4: U4): ByteStream = { stream.writeInt(u4); this }
  // appends an entire streamable object to the stream
  def <<(streamable: Streamable): ByteStream = { streamable.toStream(this) }
  // appends a sequence of streamable objects
  def <<(seq: Seq[Streamable]): ByteStream = {
    seq.foreach(_.toStream(this))
    this
  }
  // appends an entire other byte stream to the stream
  def <<(bs: ByteStream): ByteStream = {
    bs.stream.flush
    bs.bytes.writeTo(this.stream)
    this
  }

  def size: Int = stream.size

  def writeToFile(fileName: String): Unit = {
    import java.io.FileOutputStream

    val fileStream = new FileOutputStream(fileName)
    stream.flush
    bytes.writeTo(fileStream)
    fileStream.close()
  }
}
