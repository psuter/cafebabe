package cafebabe

/** A code handler contains methods to help the generation of method bodies.
 * The general usage is to generate abstract byte codes, then freeze the code,
 * which as a consequence generates the proper bytes in the CodeAttributeInfo.
 * Information is added to the constant pool during the ABS generation already.
 * <code>CodeHandler</code>s should not be created manually, but rather obtained
 * from the corresponding <code>MethodHandler</code>. */
class CodeHandler private[cafebabe](c: CodeAttributeInfo, cp: ConstantPool, val paramTypes: String, val isStatic : Boolean) {
  import ClassFileTypes._
  import AbstractByteCodes._
  import ByteCodes._

  private val code: CodeAttributeInfo = c
  protected[cafebabe] val constantPool: ConstantPool = cp
  // will be built backwards and reversed at the end...
  private var abcList: List[AbstractByteCode] = Nil
  private var frozen: Boolean = false
  protected[cafebabe] def isFrozen : Boolean = frozen

  private def append(abc: AbstractByteCode): Unit = if(!frozen) {
    abcList = abc :: abcList
  }

  def <<(abcGen: AbstractByteCodeGenerator): CodeHandler = {
    abcGen(this)
  }

  def <<(abc: AbstractByteCode): CodeHandler = {
    append(abc)
    this
  }

  // Helpers to get slots.
  private val argTypesAndBytes : Seq[(String,Int)] = {
    val bc = typesToTypesAndBytes(paramTypes)
    // that's a dirty trick, but this info is very private..
    // only used for the ArgLoad ABC.
    if(isStatic) bc else (("L;", 1) +: bc)
  }
  protected[cafebabe] val argSlotMap : Map[Int,(String,Int)] = {
    var acc : Int = 0
    (for(((tpe,sz), arg) <- argTypesAndBytes.zipWithIndex) yield {
      val s = acc
      acc += sz
      (arg, (tpe,s))
    }).toMap
  }
  private var locals: Int = argTypesAndBytes.unzip._2.sum

  /** Get a slot for a var that fits in one byte (all but `double` or `long`). */
  def getFreshVar: Int = getFreshVar(1)

  /** Get a slot for a var whose type has the JVM representation `tpe`. */
  def getFreshVar(tpe : String) : Int = getFreshVar(typeToByteCount(tpe))

  /** Get a slot for var that fits in `n` bytes. `n` must be `0` or `1`. */
  def getFreshVar(n : Int) : Int = {
    if(!(n == 1 || n == 2)) {
      throw new IllegalArgumentException("Slot for variables can only be of 1 or 2 bytes.")
    }
    val ret = locals
    locals += n
    ret
  }

  @deprecated("freeVar no longer has any effect", "1.3")
  def freeVar(id: Int): Unit = { }

  // helper to get fresh label names
  private var labelCounts = new scala.collection.mutable.HashMap[String,Int]
  def getFreshLabel(prefix: String): String = {
    val postfix: Int = labelCounts.getOrElse(prefix, {
      labelCounts(prefix) = 0
      0
    })
    val name = prefix + '_' + postfix
    labelCounts(prefix) = postfix + 1
    name
  }

  /** "Freezes" the code: maxLocals is computed, abstract byte codes are turned
   *  into concrete ones. This includes computation of the label offsets. */
  def freeze: Unit = if(frozen) {} else {
    import scala.collection.mutable.{Map=>MutableMap}
    abcList = abcList.reverse
    frozen = true
    code.maxLocals = locals

    var pc: Int = 0
    // In the first pass, we collect the positions of all labels.
    // We also store line numbers information.
    var lastLineNumber : Int = Int.MaxValue
    val labels : MutableMap[String,Int] = MutableMap.empty
    val lineInfo : MutableMap[Int,Int] = MutableMap.empty
    for(abc <- abcList) {
      abc match {
        case LineNumber(ln) if ln == lastLineNumber => ;
        case LineNumber(ln) => {
          lastLineNumber = ln
          lineInfo(pc) = ln
        }
        case Label(name) => labels(name) = pc
        case _ => ;
      }
      pc = pc + abc.size
    }

    // in the second pass, we set the jump offsets.
    pc = 0
    for(abc <- abcList) {
      abc match {
        case co: ControlOperator => {
          co.offset = (labels.getOrElse(co.target, 0) - pc)
        }
        case _ => ;
      }
      pc = pc + abc.size
    }

    // we build the line number table.
    if(!lineInfo.isEmpty) {
      val lnta = new LineNumberTableAttributeInfo(constantPool.addString("LineNumberTable"))
      lnta.setEntries(lineInfo.toSeq)
      code.attributes = lnta +: code.attributes
    }

    // we now compute the maximum stack height.
    code.maxStack = computeMaxStack(abcList)

    // finally, we dump the code.
    abcList.foreach(code.code << _)
  }

  def computeMaxStack(abcList: List[AbstractByteCode]): U2 = {
    var actualSize = abcList.map(_.size).foldLeft(0)(_+_)
    var codeArray = new Array[AbstractByteCode](actualSize)

    var pc = 0
    abcList.foreach(abc => { codeArray(pc) = abc; pc += abc.size })

    val heightArray = new Array[Int](actualSize)
    for(i <- (0 until actualSize)) { heightArray(i) = -99 }

    def setHeight(from: Int, there: Int): Unit = {
      if(there < 0) { print0(abcList); sys.error("Negative stack height at pc=" + from + " (which is " + codeArray(from) + ")") }
      if(heightArray(from) != -99) {
        if(heightArray(from) == there) {
          return
        } else {
          print0(abcList)
          sys.error("Inconsistent stack height at pc=" + from + "(" + heightArray(from) + " and " + there + ")")
        }
      }
      var pc = from
      heightArray(pc) = there

      codeArray(pc) match {
        case WIDE => sys.error("Wide is unsupported for now.")
        case RETURN => if(there != 0) sys.error("Non-empty stack after return in void method")
        case ARETURN | DRETURN | FRETURN | IRETURN | LRETURN => {
          if(there + codeArray(pc).asInstanceOf[ByteCode].stackEffect.get != 0)
            sys.error("Stack not empty after return.")
        }
        case bc: ByteCode if !bc.stackEffect.isEmpty => setHeight(from + bc.length.get, there + bc.stackEffect.get)
        case GETFIELD => codeArray(pc+1) match {
          case RawBytes(idx) => setHeight(from + 3, (there + constantPool.getFieldSize(idx)) - 1)
          case _ => sys.error("Expected RawBytes after GETFIELD.")
        }
        case GETSTATIC => codeArray(pc+1) match {
          case RawBytes(idx) => setHeight(from + 3, there + constantPool.getFieldSize(idx))
          case _ => sys.error("Expected RawBytes after GETSTATIC.")
        }
        case PUTFIELD => codeArray(pc+1) match {
          case RawBytes(idx) =>
//	I have a feeling this is incorrect:    setHeight(from + 3, -(there + constantPool.getFieldSize(idx) + 1))
	    setHeight(from + 3, there - constantPool.getFieldSize(idx) - 1) // Better?
          case _ => sys.error("Expected RawBytes after PUTFIELD.")
        }
        case PUTSTATIC => codeArray(pc+1) match {
          case RawBytes(idx) => // setHeight(from + 3, -(there + constantPool.getFieldSize(idx)))
	    setHeight(from + 3, there - constantPool.getFieldSize(idx))
          case _ => sys.error("Expected RawBytes after PUTSTATIC.")
        }
        case INVOKEVIRTUAL | INVOKESPECIAL => codeArray(pc+1) match {
          case RawBytes(idx) => {
            val se = constantPool.getMethodEffect(idx) - 1
            setHeight(from + 3, there + se)
          }
          case _ => sys.error("Expected RawBytes after INVOKEVIRTUAL/INVOKESPECIAL.")
        }
        case INVOKESTATIC => codeArray(pc+1) match {
          case RawBytes(idx) => {
            val se = constantPool.getMethodEffect(idx)
            setHeight(from + 3, se)
          }
          case _ => sys.error("Expected RawBytes after INVOKESTATIC.")
        }
        case g @ Goto(_) => setHeight(from + g.offset, there)
        case co : ControlOperator => {
          setHeight(from + co.offset, there + co.opCode.stackEffect.get)
          setHeight(from + co.opCode.length.get, there + co.opCode.stackEffect.get)
        }
        case other @ _ => sys.error("Computation of stack height unsupported for " + other)
      }
    }

    setHeight(0, 0)
    val max: Int = heightArray.max
    max.asInstanceOf[U2]
  }

  def print: Unit = if(!frozen) print0(abcList.reverse)

  private def print0(seq: Seq[AbstractByteCode]): Unit = {
    seq.foreach(_ match {
      case Label(name) => println(name + ":")
      case other @ _ => println("    " + other)
    })
  }


}
