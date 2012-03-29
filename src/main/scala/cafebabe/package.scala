package object cafebabe {
  /** Converts a string representing multiple JVM types into a sequence of
   * integers representing their respective sizes in bytes. */
  def typesToByteCounts(types : String) : Seq[Int] = {
    var s : String = types
    var lst : List[Int] = Nil
    while(!s.isEmpty) {
      val (c,ns) = byteCountRec(s)
      s = ns
      lst = c :: lst 
    } 
    lst.reverse
  }

  /** Used to compute for instance the stack effect of method invocations or
    * the number of slots required by for method arguments. In reality, a hackish
    * parser. */
  def typesToByteCount(types : String) : Int = {
    var s : String = types
    var c : Int = 0
    while(!s.isEmpty) {
      val (inc,ns) = byteCountRec(s)
      s = ns
      c += inc
    }
    c
  }

  /** Returns the number of required to store a value of a given type, in JVM
    * notation. */
  def typeToByteCount(tpe : String) : Int = {
    val (c,r) = byteCountRec(tpe)
    if(!r.isEmpty) {
      sys.error("Malformed type (sub)string: " + r)
    }
    c
  }

  private val MethSigRE = """\((.*)\)(.*)""".r
  private[cafebabe] def methodSignatureToStackEffect(sig : String) : Int = sig match {
    case MethSigRE(args,ret) => typeToByteCount(ret) - typesToByteCount(args)
    case _ => sys.error("Malformed method signature: " + sig)
  }

  // the meat of the parser
  private def byteCountRec(s : String) : (Int,String) = if(s.isEmpty)  (0,s) else {
    s.head match {
      case 'B' | 'C' | 'F' | 'I' | 'S' | 'Z' => (1, s.tail)
      case 'D' | 'J' => (2, s.tail)
      case 'L' => {
        val end = s.indexOf(';')
        if(end < 0) sys.error("Malformed type (sub)string: " + s)
        (1, s.substring(end + 1, s.size))
      }
      case '[' => {
        if(s.tail.isEmpty) sys.error("Malformed type string: incomplete array type.")
        val (_, rest) = byteCountRec(s.tail)
        (1, rest) 
      }
      case _ => sys.error("Malformed type (sub)string: " + s)
    }
  }
}
