package cafebabe

import ByteCodes._
import AbstractByteCodes._

case class CodeFreezingException(message : String, abstractByteCodes : Seq[AbstractByteCode] = Nil) extends Exception {
  override def getMessage : String = {
    if(abstractByteCodes.isEmpty) {
      message
    } else {
      val b = new StringBuilder()
      b.append(message)
      b.append("\n")

      var pc = 0
      for(abc <- abstractByteCodes) abc match {
        case Label(name) =>
          b.append(name)
          b.append(":\n")

        case _ =>
          b.append("%5d %s".format(pc, abc))
          b.append("\n")
          pc += abc.size
      }

      b.toString
    }
  }
}
