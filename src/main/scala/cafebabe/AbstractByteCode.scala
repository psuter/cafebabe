package cafebabe

object AbstractByteCodes {
  import ClassFileTypes._
  import ByteCodes._

  trait AbstractByteCode extends Streamable {
    def size: Int
    def toStream(bs: ByteStream): ByteStream = sys.error("Abstract byte code cannot produce its own code. " + this)
  }

  /** Line numbers take no space in the actual bytecode. They are
   * stored (at freeze-time) in the line number table and useful
   * for debugging and for meaningful stack-traces */
  case class LineNumber(line : Int) extends AbstractByteCode {
    override val size : Int = 0
    override def toStream(bs: ByteStream): ByteStream = bs
  }

  /** A label takes no space in the actual bytecode, but serves
   * as an anchor in the stream of ABC. */
  case class Label(id: String) extends AbstractByteCode {
    override val size: Int = 0
    override def toStream(bs: ByteStream): ByteStream = bs
  }

  // To put "raw bytes" in the stream
  case class RawByte(u1: U1) extends AbstractByteCode {
    override val size: Int = 1
    override def toStream(bs: ByteStream): ByteStream = bs << u1
  }

  case class RawBytes(u2: U2) extends AbstractByteCode {
    override val size: Int = 2
    override def toStream(bs: ByteStream): ByteStream = bs << u2
  }

  abstract class ControlOperator(val opCode: ByteCode) extends AbstractByteCode {
    def size: Int = 3
    val target: String
    var offset: Int = _

    override def toStream(bs: ByteStream): ByteStream = {
      if(offset > 65536 || offset < -32768) {
        sys.error("Unsupported long jump." + this)
      } else {
        bs << opCode << offset.asInstanceOf[U2]
      }
    }
  }

  // Branching operators. GOTO_W is unsupported for now.
  case class Goto(override val target: String) extends ControlOperator(GOTO) /*{
    var wide: Boolean = true

    override def size: Int = if(wide) 5 else 3

    override def toStream(bs: ByteStream): ByteStream = {
      if(wide) {
        bs << GOTO_W << offset.asInstanceOf[U4]
      } else {
        bs << GOTO << offset.asInstanceOf[U2]
      }
    }
  }*/
  case class IfEq(override val target: String) extends ControlOperator(IFEQ)
  case class IfNe(override val target: String) extends ControlOperator(IFNE)
  case class IfLt(override val target: String) extends ControlOperator(IFLT)
  case class IfLe(override val target: String) extends ControlOperator(IFLE)
  case class IfGt(override val target: String) extends ControlOperator(IFGT)
  case class IfGe(override val target: String) extends ControlOperator(IFGE)
  case class IfNull(override val target: String) extends ControlOperator(IFNULL)
  case class IfNonNull(override val target: String) extends ControlOperator(IFNONNULL)
  case class If_ICmpEq(override val target: String) extends ControlOperator(IF_ICMPEQ)
  case class If_ICmpNe(override val target: String) extends ControlOperator(IF_ICMPNE)
  case class If_ICmpLt(override val target: String) extends ControlOperator(IF_ICMPLT)
  case class If_ICmpLe(override val target: String) extends ControlOperator(IF_ICMPLE)
  case class If_ICmpGt(override val target: String) extends ControlOperator(IF_ICMPGT)
  case class If_ICmpGe(override val target: String) extends ControlOperator(IF_ICMPGE)
  case class If_ACmpEq(override val target: String) extends ControlOperator(IF_ACMPEQ)
  case class If_ACmpNe(override val target: String) extends ControlOperator(IF_ACMPNE)

  type AbstractByteCodeGenerator = (CodeHandler => CodeHandler)

  /** Generates code to load constants, using the appropriate method depending on the values. */
  object Ldc {
    def apply(i: Int): AbstractByteCodeGenerator = ((ch: CodeHandler) => {
      i match {
        case -1 => ch << ICONST_M1
        case 0 => ch << ICONST_0
        case 1 => ch << ICONST_1
        case 2 => ch << ICONST_2
        case 3 => ch << ICONST_3
        case 4 => ch << ICONST_4
        case 5 => ch << ICONST_5
        case _ if(i >= -128 && i <= 127) => ch << BIPUSH << RawByte(i.asInstanceOf[U1])
        case _ if(i >= -32768 && i <= 32767) => ch << SIPUSH << RawBytes(i.asInstanceOf[U2])
        case _ => ch << ldc_ref(ch.constantPool.addInt(i))
      }
    })

    def apply(f: Float): AbstractByteCodeGenerator = ((ch: CodeHandler) => {
      f match {
        case 0.0f => ch << FCONST_0
        case 1.0f => ch << FCONST_1
        case 2.0f => ch << FCONST_2
        case _ => ch << ldc_ref(ch.constantPool.addFloat(f))
      }
    })

    def apply(d: Double): AbstractByteCodeGenerator = ((ch: CodeHandler) => {
      d match {
        case 0.0 => ch << DCONST_0
        case 1.0 => ch << DCONST_1
        case _ => ch << ldc2_ref(ch.constantPool.addDouble(d))
      }
    })

    def apply(l: Long): AbstractByteCodeGenerator = ((ch: CodeHandler) => {
      l match {
        case 0L => ch << LCONST_0
        case 1L => ch << LCONST_1
        case _ => ch << ldc2_ref(ch.constantPool.addLong(l))
      }
    })

    def apply(s: String): AbstractByteCodeGenerator = ((ch: CodeHandler) => {
      ch << ldc_ref(ch.constantPool.addStringConstant(ch.constantPool.addString(s)))
    })

    def apply(c: Class[_]): AbstractByteCodeGenerator = ((ch: CodeHandler) => {
      ch << ldc_ref(ch.constantPool.addClass(ch.constantPool.addString(c.getName().replaceAll("\\.", "/"))))
    })

    private def ldc_ref(cpRef: U2): AbstractByteCodeGenerator = ((ch: CodeHandler) => {
      if(cpRef <= 0xFF) {
        ch << LDC << RawByte((cpRef & 0xFF).asInstanceOf[U1])
      } else {
        ch << LDC_W << RawBytes(cpRef)
      }
    })

    private def ldc2_ref(cpRef: U2): AbstractByteCodeGenerator = ((ch: CodeHandler) => {
      ch << LDC2_W << RawBytes(cpRef)
    })
  }

  // IINC business
  object IInc {
    def apply(index: Int, inc: Int): AbstractByteCodeGenerator = ((ch: CodeHandler) => {
      if(index <= 127 && inc >= -128 && inc <= 127) {
        ch << IINC << RawByte(index.asInstanceOf[U1]) << RawByte(inc.asInstanceOf[U1])
      } else if(index <= 32767 && inc >= -32768 && inc <= 32767) {
        ch << WIDE << IINC << RawBytes(index.asInstanceOf[U2]) << RawBytes(index.asInstanceOf[U2])
      } else {
        sys.error("Index or increment too large in IInc " + index + " " + inc)
      }
    })
  }

  // Loading and storing locals
  private def storeLoad(index: Int, name: String, bc: ByteCode,
                        bc0: ByteCode, bc1: ByteCode,
                        bc2: ByteCode, bc3: ByteCode): AbstractByteCodeGenerator = {
    (ch: CodeHandler) => index match {
      case 0 => ch << bc0
      case 1 => ch << bc1
      case 2 => ch << bc2
      case 3 => ch << bc3
      case _ if(index >= 0 && index <= 127) => ch << bc << RawByte(index.asInstanceOf[U1])
      case _ if(index >= 0 && index <= 32767) => ch << WIDE << bc << RawBytes(index.asInstanceOf[U2])
      case _ => sys.error("Invalid index in " + name + " " + index)
    }
  }

  object ALoad { def apply(index: Int) = storeLoad(index, "ALoad", ALOAD, ALOAD_0, ALOAD_1, ALOAD_2, ALOAD_3) }
  object DLoad { def apply(index: Int) = storeLoad(index, "DLoad", DLOAD, DLOAD_0, DLOAD_1, DLOAD_2, DLOAD_3) }
  object FLoad { def apply(index: Int) = storeLoad(index, "FLoad", FLOAD, FLOAD_0, FLOAD_1, FLOAD_2, FLOAD_3) }
  object ILoad { def apply(index: Int) = storeLoad(index, "ILoad", ILOAD, ILOAD_0, ILOAD_1, ILOAD_2, ILOAD_3) }
  object LLoad { def apply(index: Int) = storeLoad(index, "LLoad", LLOAD, LLOAD_0, LLOAD_1, LLOAD_2, LLOAD_3) }
  object AStore { def apply(index: Int) = storeLoad(index, "AStore", ASTORE, ASTORE_0, ASTORE_1, ASTORE_2, ASTORE_3) }
  object DStore { def apply(index: Int) = storeLoad(index, "DStore", DSTORE, DSTORE_0, DSTORE_1, DSTORE_2, DSTORE_3) }
  object FStore { def apply(index: Int) = storeLoad(index, "FStore", FSTORE, FSTORE_0, FSTORE_1, FSTORE_2, FSTORE_3) }
  object IStore { def apply(index: Int) = storeLoad(index, "IStore", ISTORE, ISTORE_0, ISTORE_1, ISTORE_2, ISTORE_3) }
  object LStore { def apply(index: Int) = storeLoad(index, "LStore", LSTORE, LSTORE_0, LSTORE_1, LSTORE_2, LSTORE_3) }

// Some magic for loading locals.

  object ArgLoad {
    /** Loads an argument by its index in the argument list. 0 is the receiver
      * for non-static methods. */
    def apply(index : Int) : AbstractByteCodeGenerator = (ch : CodeHandler) => ch.argSlotMap.get(index) match {
      case None => sys.error("Invalid argument index : " + index)
      case Some((tpe,i)) => tpe match {
        case "I" | "B" | "C" | "S" | "Z" => ch << ILoad(i)
        case "F" => ch << FLoad(i)
        case "J" => ch << LLoad(i)
        case "D" => ch << DLoad(i)
        case "V" => sys.error("Illegal argument of type `void` !?!")
        case _  => ch << ALoad(i) // this is bold :)
      }
    }
  }


  // Field access
  object GetField  { def apply(className: String, fieldName: String, fieldType: String) = accessField(GETFIELD,  className, fieldName, fieldType) }
  object GetStatic { def apply(className: String, fieldName: String, fieldType: String) = accessField(GETSTATIC, className, fieldName, fieldType) }
  object PutField  { def apply(className: String, fieldName: String, fieldType: String) = accessField(PUTFIELD,  className, fieldName, fieldType) }
  object PutStatic { def apply(className: String, fieldName: String, fieldType: String) = accessField(PUTSTATIC, className, fieldName, fieldType) }

  private def accessField(bc: ByteCode, className: String, fieldName: String, fieldType: String): AbstractByteCodeGenerator =
    (ch: CodeHandler) => {
      ch << bc << RawBytes(ch.constantPool.addFieldRef(
        ch.constantPool.addClass(ch.constantPool.addString(className)),
        ch.constantPool.addNameAndType(
          ch.constantPool.addString(fieldName),
          ch.constantPool.addString(fieldType))))
  }

  // Method invocations

  object InvokeInterface {
    def apply(className: String, methodName: String, methodSig: String): AbstractByteCodeGenerator =
      _ << invokeMethod(INVOKEINTERFACE, className, methodName, methodSig) <<
           RawByte(methodSignatureArgStackEffect(methodSig) + 1) << RawByte(0)
  }

  object InvokeSpecial {
    def apply(className: String, methodName: String, methodSig: String): AbstractByteCodeGenerator =
      invokeMethod(INVOKESPECIAL, className, methodName, methodSig)
  }

  object InvokeStatic {
    def apply(className: String, methodName: String, methodSig: String): AbstractByteCodeGenerator =
      invokeMethod(INVOKESTATIC, className, methodName, methodSig)
  }

  object InvokeVirtual {
    def apply(className: String, methodName: String, methodSig: String): AbstractByteCodeGenerator =
      invokeMethod(INVOKEVIRTUAL, className, methodName, methodSig)
  }

  private def invokeMethod(bc: ByteCode, className: String, methodName: String, methodSig: String): AbstractByteCodeGenerator = {
    (ch: CodeHandler) => {
      val addMethodRef = if (bc == INVOKEINTERFACE) ch.constantPool.addInterfaceMethodRef _ else ch.constantPool.addMethodRef _
      ch << bc << RawBytes(addMethodRef(
        ch.constantPool.addClass(ch.constantPool.addString(className)),
        ch.constantPool.addNameAndType(
          ch.constantPool.addString(methodName),
          ch.constantPool.addString(methodSig))))
    }
  }

  // misc

  object New {
    def apply(className: String) : AbstractByteCodeGenerator = {
      (ch: CodeHandler) => {
        ch << NEW << RawBytes(ch.constantPool.addClass(ch.constantPool.addString(className)))
      }
    }
  }

  object DefaultNew {
    def apply(className: String): AbstractByteCodeGenerator = {
      (ch: CodeHandler) => {
        ch << NEW << RawBytes(ch.constantPool.addClass(ch.constantPool.addString(className))) << DUP << InvokeSpecial(className, "<init>", "()V")
      }
    }
  }

  object InstanceOf {
    def apply(className: String): AbstractByteCodeGenerator = {
      (ch: CodeHandler) => {
        ch << INSTANCEOF << RawBytes(ch.constantPool.addClass(ch.constantPool.addString(className)))
      }
    }
  }

  object CheckCast {
    def apply(className: String): AbstractByteCodeGenerator = {
      (ch: CodeHandler) => {
        ch << CHECKCAST << RawBytes(ch.constantPool.addClass(ch.constantPool.addString(className)))
      }
    }
  }

  object NewArray {
    def apply(arrayType: String): AbstractByteCodeGenerator = { // For objects
      (ch: CodeHandler) => {
	ch << ANEWARRAY << RawBytes(ch.constantPool.addClass(ch.constantPool.addString(arrayType)))
      }
    }

    def apply(tpe: Int): AbstractByteCodeGenerator = { // Primitive types
      (ch: CodeHandler) => {
	ch << NEWARRAY << RawByte(tpe)
      }
    }

    def primitive(tpe: String): AbstractByteCodeGenerator = { // with type string
      apply(types(tpe))
    }

    val types = Map(
      "T_BOOLEAN" -> 4,
      "T_CHAR" -> 5,
      "T_FLOAT" -> 6,
      "T_DOUBLE" -> 7,
      "T_BYTE" -> 8,
      "T_SHORT" -> 9,
      "T_INT" -> 10,
      "T_LONG" -> 11
    )
  }

  // So that we can annotate our generated code before freezing
  case class Comment(comment: String) extends AbstractByteCode {
    override val size: Int = 0
    override def toStream(bs: ByteStream): ByteStream = bs
  }
}

