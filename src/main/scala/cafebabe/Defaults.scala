package cafebabe

object Defaults {
  import ClassFileTypes._
  import Flags._

  val defaultMagic: U4 = 0xCAFEBABE
  val defaultMinor: U2 = 0
  val defaultMajor: U2 = 49 // J2SE 6.0=50, J2SE 5.0=49, JDK 1.4=48, JDK 1.3=47, JDK 1.2=46, JDK 1.1=45

  val defaultClassAccessFlags: U2  = CLASS_ACC_PUBLIC | CLASS_ACC_SUPER
  val defaultMethodAccessFlags: U2 = METHOD_ACC_PUBLIC
  val defaultFieldAccessFlags: U2  = FIELD_ACC_PROTECTED

  val constructorName: String = "<init>"
  val constructorSig: String = "()V"
}
