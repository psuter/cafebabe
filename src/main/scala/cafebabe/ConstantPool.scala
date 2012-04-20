package cafebabe

class ConstantPool extends Streamable {
  import ClassFileTypes._

  import scala.collection.mutable.HashMap

  /** The following maps keep track of the constants already added to the pool to avoid duplicates. */
  private val intMap: HashMap[Int,U2]       = new HashMap[Int,U2]
  private val floatMap: HashMap[Float,U2]   = new HashMap[Float,U2]
  private val longMap: HashMap[Long,U2]     = new HashMap[Long,U2]
  private val doubleMap: HashMap[Double,U2] = new HashMap[Double,U2]
  private val stringMap: HashMap[String,U2] = new HashMap[String,U2]       // all internal strings
  private val stringConstMap: HashMap[U2,U2] = new HashMap[U2,U2]          // string constants
  private val classMap: HashMap[U2,U2] = new HashMap[U2,U2]
  private val fieldRefMap: HashMap[(U2,U2),U2] = new HashMap[(U2,U2),U2]
  private val methodRefMap: HashMap[(U2,U2),U2] = new HashMap[(U2,U2),U2]
  private val nameAndTypeMap: HashMap[(U2,U2),U2] = new HashMap[(U2,U2),U2]

  /** The list of all entries in that constant pool. */
  private var entries: List[CPEntry] = Nil

  /** Returns the number of entries. */
  def size: U2 = entries.length

  private var nextIndex: U2 = 1

  /** Adds an entry into the constant pool and returns its index. */
  private def addEntry(entry: CPEntry): U2 = {
    entries = entries ::: (entry :: Nil)
    val ret = nextIndex
    nextIndex = nextIndex + (entry match {
      case e: CPLongInfo => 2
      case e: CPDoubleInfo => 2
      case _ => 1
    })
    ret
  }

  /** Finds the nth entry. */
  private def entryAt(idx: Int): CPEntry = {
    def ea(idx: Int, lst: List[CPEntry]): CPEntry = {
      if(idx == 0) lst.head else lst.head match {
        case e: CPLongInfo => ea(idx - 2, lst.tail)
        case e: CPDoubleInfo => ea(idx - 2, lst.tail)
        case _ => ea(idx - 1, lst.tail)
      }
    }

    ea(idx-1, entries)
  }

  /** The following methods add constants to the pool, using hashmaps to avoid duplicates and properly encoding the values. */
  def addInt(i: Int): U2 = intMap.getOrElse(i, {
    val idx = addEntry(CPIntegerInfo(encodeInt(i)))
    intMap += (i -> idx)
    idx
  })
  def addFloat(f: Float): U2 = floatMap.getOrElse(f, {
    val idx = addEntry(CPFloatInfo(encodeFloat(f)))
    floatMap += (f -> idx)
    idx
  })

  def addLong(l: Long): U2 = longMap.getOrElse(l, {
    val enc = encodeLong(l)
    val idx = addEntry(CPLongInfo(enc._1, enc._2))
    longMap += (l -> idx)
    idx
  })
  def addDouble(d: Double): U2 = doubleMap.getOrElse(d, {
    val enc = encodeDouble(d)
    val idx = addEntry(CPDoubleInfo(enc._1, enc._2))
    doubleMap += (d -> idx)
    idx
  })
  def addString(s: String): U2 = stringMap.getOrElse(s, {
    val idx = addEntry(CPUtf8Info(encodeString(s)).setSource(s))
    stringMap += (s -> idx)
    idx
  })
  def addStringConstant(strID: U2): U2 = stringConstMap.getOrElse(strID, {
    val idx = addEntry(CPStringInfo(strID))
    stringConstMap += (strID -> idx)
    idx
  })
  def addClass(nameID: U2): U2 = classMap.getOrElse(nameID, {
    val idx = addEntry(CPClassInfo(nameID))
    classMap += (nameID -> idx)
    idx
  })
  def addFieldRef(classID: U2, natID: U2): U2 = fieldRefMap.getOrElse((classID,natID), {
    val idx = addEntry(CPFieldRefInfo(classID,natID))
    fieldRefMap += ((classID,natID) -> idx)
    idx
  })
  def addMethodRef(classID: U2, natID: U2): U2 = methodRefMap.getOrElse((classID,natID), {
    val idx = addEntry(CPMethodRefInfo(classID,natID))
    methodRefMap += ((classID,natID) -> idx)
    idx
  })
  def addInterfaceMethodRef(classID: U2, natID: U2): U2 = methodRefMap.getOrElse((classID,natID), {
    val idx = addEntry(CPInterfaceMethodRefInfo(classID,natID))
    methodRefMap += ((classID,natID) -> idx)
    idx
  })
  def addNameAndType(nameID: U2, typeID: U2): U2 = nameAndTypeMap.getOrElse((nameID,typeID), {
    val idx = addEntry(CPNameAndTypeInfo(nameID,typeID))
    nameAndTypeMap += ((nameID,typeID) -> idx)
    idx
  })

  /** The following methods encode numerical values into their byte representation. */
  private def encodeInt(i: Int): U4 = i
  private def encodeFloat(f: Float): U4 = java.lang.Float.floatToIntBits(f)
  private def encodeLong(l: Long): (U4,U4) = ((l >>> 32).asInstanceOf[U4], (l & 0xFFFFFFFF).asInstanceOf[U4])
  private def encodeDouble(d: Double): (U4,U4) = encodeLong(java.lang.Double.doubleToLongBits(d))

  /** Encodes a string into the unusual UTF8-like encoding used in the class file format. */
  private def encodeString(s: String): Seq[U1] = {
    import scala.collection.mutable.ArrayBuffer
    val bytes = ArrayBuffer.empty[U1]

    for(c: Char <- s) {
      if(c >= 0x0001 && c <= 0x007F) {
        bytes.append(c)
      } else if(c >= 0x0800) {
        bytes.append(0xE0 | ((c >>> 12) & 0x0F))
        bytes.append(0x80 | ((c >>> 6)  & 0x3F))
        bytes.append(0x80 |          (c & 0x3F))
      } else {
        bytes.append(0xC0 | ((c >>> 6) & 0x1F))
        bytes.append(0x80 |         (c & 0x3F))
      }
    }
    bytes
  }

  def getFieldSize(idx: U2): Int = entryAt(idx) match {
    case CPFieldRefInfo(_, natid) => {
      val strDesc: String = entryAt(entryAt(natid).asInstanceOf[CPNameAndTypeInfo].descriptorIndex).asInstanceOf[CPUtf8Info].getSource
      strDesc match {
        case "D" | "J" => 2
        case _ => 1
      }
    }
    case _ => sys.error("getFieldSize: no field info at given index.")
  }

  def getMethodEffect(idx: U2): Int = entryAt(idx) match {
    case CPMethodRefInfo(_, natid) => {
      val strDesc: String = entryAt(entryAt(natid).asInstanceOf[CPNameAndTypeInfo].descriptorIndex).asInstanceOf[CPUtf8Info].getSource
      methodSignatureToStackEffect(strDesc)
    }
    case CPInterfaceMethodRefInfo(_, natid) => {
      val strDesc: String = entryAt(entryAt(natid).asInstanceOf[CPNameAndTypeInfo].descriptorIndex).asInstanceOf[CPUtf8Info].getSource
      methodSignatureToStackEffect(strDesc)
    }
    case _ => sys.error("getMethodEffect: no method ref info at given index.")
  }

  def toStream(stream: ByteStream): ByteStream = {
    stream << nextIndex.asInstanceOf[U2] << entries
  }
}
