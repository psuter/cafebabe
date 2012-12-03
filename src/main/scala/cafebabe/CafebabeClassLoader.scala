package cafebabe

import scala.collection.mutable.{Map=>MutableMap}

/** A `ClassLoader` with the capability for loading cafebabe
 *  `ClassFile`s directly from memory. */
class CafebabeClassLoader extends ClassLoader {
  private val classes : MutableMap[String,Array[Byte]] = MutableMap.empty

  def register(classFile : ClassFile) {
    val byteStream = (new ByteStream) << classFile
    classes(classFile.className) = byteStream.getBytes
  }

  override def findClass(name : String) : Class[_] = {
    classes.get(name) match {
      case Some(ba) => defineClass(name, ba, 0, ba.length)
      case None => super.findClass(name)
    }
  }

  def newInstance(name : String) : AnyRef = {
    val klass = this.loadClass(name)
    klass.newInstance().asInstanceOf[AnyRef]
  }
}
