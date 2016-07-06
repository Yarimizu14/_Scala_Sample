import scala.language.implicitConversions

object HumanImports {
  class HumanExt(h: Human) {
    def isYoung: Boolean = h.age < 20
  }
  implicit def humanToHumanExt(h: Human)
    = new HumanExt(h)
}

object PoundImports {
  class PoundNumber(kg: Double) {
    def toPound: Double = kg * 2.20462
  }
  implicit def kgToPoundNumber(kg: Double)
    = new PoundNumber(kg)
}
