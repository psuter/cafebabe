package object cafebabe {
  /** Converts a string representing multiple JVM types into a sequence of
   * integers representing their respective sizes in bytes. */
  def typesToByteCounts(types : String) : Seq[Int] = {
    var s : String = types
    var lst : List[Int] = Nil
    while(!s.isEmpty) {
      val (c,_,ns) = parseRec(s)
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
      val (inc,_,ns) = parseRec(s)
      s = ns
      c += inc
    }
    c
  }

  /** Returns the number of required to store a value of a given type, in JVM
    * notation. */
  def typeToByteCount(tpe : String) : Int = {
    val (c,_,r) = parseRec(tpe)
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
  private[cafebabe] def methodSignatureArgStackEffect(sig: String) : Int = sig match {
    case MethSigRE(args,ret) =>
      typesToByteCount(args) // does not account for 'this' since we don't know if this is static
    case _ => sys.error("Malformed method signature: " + sig)
  }

  // the meat of the parser
  private def parseRec(s : String) : (Int,String,String) = if(s.isEmpty)  (0,s,s) else {
    s.head match {
      case 'B' | 'C' | 'F' | 'I' | 'S' | 'Z' => (1, s.head.toString, s.tail)
      case 'D' | 'J' => (2, s.head.toString, s.tail)
      case 'V' => (0, s.head.toString, s.tail)  // can't really be an argument type.. Oh well.
      case 'L' => {
        val end = s.indexOf(';')
        if(end < 0) sys.error("Malformed type (sub)string: " + s)
        (1, s.substring(0, end), s.substring(end + 1, s.size))
      }
      case '[' => {
        if(s.tail.isEmpty) sys.error("Malformed type string: incomplete array type.")
        val (_, ss, rest) = parseRec(s.tail)
        (1, "[" + ss, rest)
      }
      case _ => sys.error("Malformed type (sub)string: " + s)
    }
  }

  // This one I want to keep internal for now.. I don't think *returning*
  // Strings representing JVM types is good practice. (reading them is OK I'd
  // say).
  private[cafebabe] def typesToTypesAndBytes(types : String) : Seq[(String,Int)] = {
    var s : String = types
    var lst : List[(String,Int)] = Nil
    while(!s.isEmpty) {
      val (c,ss,ns) = parseRec(s)
      s = ns
      lst = (ss,c) :: lst
    }
    lst.reverse
  }
}
