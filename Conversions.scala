import scala.language.implicitConversions

object HumanImports {
  class HumanExt(h: Human) {
    def isYoung: Boolean = h.age < 20
  }
  implicit def humanToHumanExt(h: Human)
    = new HumanExt(h)
}
