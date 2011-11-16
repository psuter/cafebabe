package cafebabe

import ClassFileTypes._

class LineNumberTableAttributeInfo(val nameIndex : U2) extends AttributeInfo(nameIndex, Nil) {
  private var _entries : Map[Int,Int] = Map.empty

  private def numEntries = _entries.size

  def setEntries(entries : Seq[(Int,Int)]) : Unit = {
    _entries = entries.toMap
  }

  override def toStream(stream : ByteStream) : ByteStream = {
    val ne = numEntries
    // println("Num entries : " + ne)
    // println("Computed    : " + (2 + ne * 4))

    stream << nameIndex << ((2 + ne * 4) : U4) << (ne : U2)
    for((pc,ln) <- _entries.toSeq.sortBy(_._1)) {
      stream << (pc : U2) << (ln : U2)
    }
    stream
  }

  override def size : Int = 8 + numEntries * 4
}
