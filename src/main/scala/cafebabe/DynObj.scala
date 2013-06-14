package cafebabe

import scala.language.dynamics

/** Do not rely on this object, as it will change/be renamed, etc.
 * The goal is for `CafebabeClassLoader`s to generate `DynObj`s or
 * equivalent, so that users of dynamic class generation have to
 * write explicit reflection code as little as possible. */
class DynObj private[cafebabe](base : AnyRef) extends Dynamic {
  // getDeclaredMethods to build method map, etc.

  def applyDynamic(name: String)(args: Any*) : Any = {
    println("applyDynamic: " + name)
  }
}
