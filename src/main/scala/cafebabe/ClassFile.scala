package cafebabe

class ClassFile(val className: String, parentName: Option[String]) extends Streamable {
  import ClassFileTypes._
  import Defaults._
    
  private var magic: U4 = defaultMagic
  private var minor: U2 = defaultMinor
  private var major: U2 = defaultMajor

  private var constantPool = new ConstantPool()
  private lazy val codeNameIndex: U2 = constantPool.addString("Code")
  private lazy val sourceFileNameIndex: U2 = constantPool.addString("SourceFile")

  private var accessFlags: U2 = defaultClassAccessFlags
  
  private val thisClass: U2 = constantPool.addClass(constantPool.addString(className))
  
  private val superClassName: String = parentName match {
    case None => "java/lang/Object"
    case Some(name) => name
  }
  private var superClass: U2 = constantPool.addClass(constantPool.addString(superClassName))
  
  private var fields: List[FieldInfo] = Nil
  private var methods: List[MethodInfo] = Nil
  
  // TODO
  private var interfacesCount: U2 = 0
  private var interfaces: List[U1] = Nil

  private var attributes : List[AttributeInfo] = Nil
  
  private var _srcNameWasSet = false
  def setSourceFile(sf : String) : Unit = {
    if(_srcNameWasSet) {
      sys.error("Cannot set the source file attribute twice.")
    }
    _srcNameWasSet = true
    val idx = constantPool.addString(sf)
    attributes = SourceFileAttributeInfo(sourceFileNameIndex, idx) :: attributes
  }

  /** Adds a field to the class, using the default flags and no attributes. */
  def addField(tpe: String, name: String): FieldHandler = {
    val accessFlags: U2 = defaultFieldAccessFlags
    val nameIndex: U2 = constantPool.addString(name)
    val descriptorIndex: U2 = constantPool.addString(stringToDescriptor(tpe))
    val inf = FieldInfo(accessFlags, nameIndex, descriptorIndex, Nil)
    fields = fields ::: (inf :: Nil)
    new FieldHandler(inf, constantPool)
  }
  
  /** Adds a method with arbitrarily many arguments, using the default flags and no */
  def addMethod(retTpe: String, name: String, args: String*): MethodHandler =
    addMethod(retTpe,name,args.toList)

  def addMethod(retTpe: String, name: String, args: List[String]): MethodHandler = {
    val accessFlags: U2 = defaultMethodAccessFlags
    val nameIndex: U2 = constantPool.addString(name)
    val descriptorIndex: U2 = constantPool.addString(args.toList.mkString("(", "", ")") + retTpe)
    val code = CodeAttributeInfo(codeNameIndex)
    val inf = MethodInfo(accessFlags, nameIndex, descriptorIndex, List(code))
    methods = methods ::: (inf :: Nil)

    val concatArgs = args.mkString("")
    
    def countArgs(str: String): Int = if(str == "") 0 else str.charAt(0) match {
      case 'I' | 'J' | 'Z' | 'B' | 'F' | 'D' => 1 + countArgs(str.substring(1))
      case '[' => countArgs(str.substring(1))
      case 'L' => 1 + countArgs(str.substring(str.indexOf(';') + 1))
      case other @ _ => sys.error("Invalid character in method signature: " + other)
    }

    new MethodHandler(inf, code, constantPool, countArgs(concatArgs)+1)
  }
  
  /** Adds the main method */
  def addMainMethod: MethodHandler = {
    val handler = addMethod("V", "main", "[Ljava/lang/String;")
    handler.setFlags(Flags.METHOD_ACC_PUBLIC | Flags.METHOD_ACC_STATIC)
    handler
  }
  
  /** Adds a default constructor. */
  def addDefaultConstructor: MethodHandler = {
    val accessFlags: U2 = Flags.METHOD_ACC_PUBLIC
    val nameIndex: U2 = constantPool.addString(constructorName)
    val descriptorIndex: U2 = constantPool.addString(constructorSig)
    val code = CodeAttributeInfo(codeNameIndex)
    val inf = MethodInfo(accessFlags, nameIndex, descriptorIndex, List(code))
    methods = methods ::: (inf :: Nil)
    val mh = new MethodHandler(inf, code, constantPool, 1)
    
    import ByteCodes._
    import AbstractByteCodes._
    
    mh.codeHandler << ALOAD_0
    mh.codeHandler << InvokeSpecial(superClassName, constructorName, constructorSig)
    mh.codeHandler << RETURN
    mh.codeHandler.freeze
    mh
  }
    
  /** Writes the binary representation of this class file to a file. */
  def writeToFile(fileName : String) : Unit = {
    // The stream we'll ultimately use to write the class file data
    val byteStream = new ByteStream
    byteStream << this
    byteStream.writeToFile(fileName)
  }

  /** Loads the class using the current class loader. */
  def dynamicallyLoad : Unit = {
    throw new Error("Not supported yet.")
    /*
    val byteStream = (new ByteStream) << this
    val bytes : Array[Byte] = byteStream.getBytes
    CustomClassLoader.registerClass(className, bytes)
    */
  }
  
  def toStream(byteStream: ByteStream): ByteStream = {
    byteStream <<
      magic <<
      minor <<
      major <<
      constantPool <<
      accessFlags <<
      thisClass <<
      superClass <<
      interfacesCount <<
      fields.size.asInstanceOf[U2] << fields <<
      methods.size.asInstanceOf[U2] << methods <<
      attributes.size.asInstanceOf[U2] << attributes

  }
  
  def stringToDescriptor(s: String) = s
}


