package cafebabe

import scala.collection.mutable.{Map=>MutableMap}

object CustomClassLoader extends ClassLoader {
  private val classes : MutableMap[String,Array[Byte]] = MutableMap.empty

  def registerClass(name : String, bytes : Array[Byte]) : Unit = {
    classes(name) = bytes
  }

  override def findClass(name : String) : Class[_] = {
    classes.get(name) match {
      case Some(ba) => defineClass(name, ba, 0, ba.length)
      case None => super.findClass(name)
    }
  }
}
