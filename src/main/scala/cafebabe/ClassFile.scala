package cafebabe

/** A <code>ClassFile</code> object is an abstract representation of all the
 * information that will be written to a <code>.class</code> file.  In the Java
 * model, that generally corresponds to one class (or interface) as declared in
 * source code, however this is by no means a restriction of the platform. */
class ClassFile(val className: String, parentName: Option[String] = None) extends Streamable {
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
  private var interfaces: List[InterfaceInfo] = Nil
  private var attributes : List[AttributeInfo] = Nil

  def addInterface(name: String) {
    val nameIndex = constantPool.addClass(constantPool.addString(name))

    interfaces = InterfaceInfo(name, nameIndex) :: interfaces
  }

  private var _srcNameWasSet = false
  /** Attaches the name of the original source file to the class file. */
  def setSourceFile(sf : String) : Unit = {
    if(_srcNameWasSet) {
      sys.error("Cannot set the source file attribute twice.")
    }
    _srcNameWasSet = true
    val idx = constantPool.addString(sf)
    attributes = SourceFileAttributeInfo(sourceFileNameIndex, idx) :: attributes
  }

  /** Sets the access flags for the class. */
  def setFlags(flags : U2) : Unit = { accessFlags = flags }

  /** Returns the currently set flags. */
  def getFlags : U2 = accessFlags

  /** Adds a field to the class, using the default flags and no attributes. */
  def addField(tpe: String, name: String): FieldHandler = {
    val accessFlags: U2 = defaultFieldAccessFlags
    val nameIndex: U2 = constantPool.addString(name)
    val descriptorIndex: U2 = constantPool.addString(stringToDescriptor(tpe))
    val inf = FieldInfo(accessFlags, nameIndex, descriptorIndex, Nil)
    fields = fields ::: (inf :: Nil)
    new FieldHandler(inf, constantPool)
  }

  /** Adds a method with arbitrarily many arguments, using the default flags and no attributes. */
  def addMethod(retTpe: String, name: String, args: String*): MethodHandler = addMethod(retTpe,name,args.toList)

  def addMethod(retTpe: String, name: String, args: List[String]): MethodHandler = {
    val concatArgs = args.mkString("")

    val accessFlags: U2 = defaultMethodAccessFlags
    val nameIndex: U2 = constantPool.addString(name)
    val descriptorIndex: U2 = constantPool.addString(
      "(" + concatArgs + ")" + retTpe
    )
    val code = CodeAttributeInfo(codeNameIndex)
    val inf = MethodInfo(accessFlags, nameIndex, descriptorIndex, List(code))
    methods = methods ::: (inf :: Nil)


    new MethodHandler(inf, code, constantPool, concatArgs)
  }

  /** Adds the main method */
  def addMainMethod: MethodHandler = {
    val handler = addMethod("V", "main", "[Ljava/lang/String;")
    handler.setFlags(Flags.METHOD_ACC_PUBLIC | Flags.METHOD_ACC_STATIC)
    handler
  }

  /** Adds a constructor to the class. Constructor code should always start by invoking a constructor from the super class. */
  def addConstructor(args : String*) : MethodHandler = addConstructor(args.toList)

  def addConstructor(args : List[String]) : MethodHandler = {
    val concatArgs = args.mkString("")

    val accessFlags : U2 = Flags.METHOD_ACC_PUBLIC
    val nameIndex : U2 = constantPool.addString(constructorName)
    val descriptorIndex : U2 = constantPool.addString(
      "(" + concatArgs + ")V"
    )
    val code = CodeAttributeInfo(codeNameIndex)
    val inf = MethodInfo(accessFlags, nameIndex, descriptorIndex, List(code))
    methods = methods ::: (inf :: Nil)
    val mh = new MethodHandler(inf, code, constantPool, concatArgs)
    mh
  }

  /** Adds a default constructor. */
  def addDefaultConstructor: MethodHandler = {
    import ByteCodes._
    import AbstractByteCodes._

    val mh = addConstructor(Nil)
    mh.codeHandler << ALOAD_0
    mh.codeHandler << InvokeSpecial(superClassName, constructorName, "()V")
    mh.codeHandler << RETURN
    mh.codeHandler.freeze
    mh
  }

  /** Writes the binary representation of this class file to a file. */
  def writeToFile(fileName : String) {
    // The stream we'll ultimately use to write the class file data
    val byteStream = new ByteStream
    byteStream << this
    byteStream.writeToFile(fileName)
  }

  def registerWithClassLoader(classLoader : CafebabeClassLoader) {
    classLoader.register(this)
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
      interfaces.size.asInstanceOf[U2] << interfaces.reverse <<
      fields.size.asInstanceOf[U2] << fields <<
      methods.size.asInstanceOf[U2] << methods <<
      attributes.size.asInstanceOf[U2] << attributes

  }

  def stringToDescriptor(s: String) = s
}


