package cafebabe

object Flags {
  import ClassFileTypes._

  val CLASS_ACC_PUBLIC : U2        = 0x0001
  val CLASS_ACC_FINAL : U2         = 0x0010
  val CLASS_ACC_SUPER : U2         = 0x0020
  val CLASS_ACC_INTERFACE : U2     = 0x0200
  val CLASS_ACC_ABSTRACT : U2      = 0x0400

  val FIELD_ACC_PUBLIC : U2        = 0x0001
  val FIELD_ACC_PRIVATE : U2       = 0x0002
  val FIELD_ACC_PROTECTED : U2     = 0x0004
  val FIELD_ACC_STATIC : U2        = 0x0008
  val FIELD_ACC_FINAL : U2         = 0x0010
  val FIELD_ACC_VOLATILE : U2      = 0x0040
  val FIELD_ACC_TRANSIENT : U2     = 0x0080

  val METHOD_ACC_PUBLIC : U2       = 0x0001
  val METHOD_ACC_PRIVATE : U2      = 0x0002
  val METHOD_ACC_PROTECTED : U2    = 0x0004
  val METHOD_ACC_STATIC : U2       = 0x0008
  val METHOD_ACC_FINAL : U2        = 0x0010
  val METHOD_ACC_SYNCHRONIZED : U2 = 0x0020
  val METHOD_ACC_NATIVE : U2       = 0x0100
  val METHOD_ACC_ABSTRACT : U2     = 0x0400
  val METHOD_ACC_STRICT : U2       = 0x0800
}
