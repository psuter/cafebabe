package cafebabe

import scala.collection.mutable.{Map=>MutableMap}

/** A `ClassLoader` with the capability for loading cafebabe
 *  `ClassFile`s directly from memory. */
class CafebabeClassLoader(parent : ClassLoader) extends ClassLoader(parent) {
  def this() {
    this(ClassLoader.getSystemClassLoader())
  }

  private val classBytes : MutableMap[String,Array[Byte]] = MutableMap.empty

  def register(classFile : ClassFile) {
    val name = classFile.className
    if(classBytes.isDefinedAt(name)) {
      throw new IllegalArgumentException("Cannot define the same class twice (%s).".format(name))
    }

    val byteStream = (new ByteStream) << classFile
    classBytes(name) = byteStream.getBytes
  }

  override def findClass(name : String) : Class[_] = {
    classBytes.get(name) match {
      case Some(ba) =>
        defineClass(name, ba, 0, ba.length)

      case None => super.findClass(name)
    }
  }

  def newInstance(name : String) : AnyRef = {
    val klass = this.loadClass(name)
    klass.newInstance().asInstanceOf[AnyRef]
  }
}
