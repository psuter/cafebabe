package cafebabe

object ClassFileTypes {
  type U1 = Byte
  type U2 = Short
  type U4 = Int

  implicit def IntToU1(i: Int): U1 = i.asInstanceOf[U1]
  implicit def IntToU2(i: Int): U2 = i.asInstanceOf[U2]
  implicit def IntToU4(i: Int): U4 = i.asInstanceOf[U4]
  implicit def CharToU1(c: Char): U1 = c.asInstanceOf[U1]
}