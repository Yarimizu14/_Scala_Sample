import Math.{ pow }

abstract class Human {
  def age: Int
  def tall: Double
  def weight: Double
  def bmi: Double = weight / pow(tall / 100, 2)
}

case class Male(val age: Int, val tall: Double, val weight: Double) extends Human

case class Female(val age: Int, val tall: Double, val weight: Double) extends Human

object Statistics extends App {

  // 1. Function is first-class object
  val func: (Int) => Int = (x :Int) => x * 2;
  //val func = (x :Int) => x * 2;
  println(func(2))

   // Input source
   val source: List[Human] = List(Male(31, 176.0, 63.0), Female(28, 160, 50.0), Male(25, 170.0, 56.0))

   val func2: (Human) => Boolean = (h: Human) => h.age > 4
   //val r: List[Human] = source.filter(_.age > 4)
   val r: List[Human] = source.filter(func2)
   println(r)
   val r2: List[Human] = source.filter((s: Human) => isMale(s))
   println(r2)

   val f = filterByAge(source)
   val r3 = f(4)
   println(r3)

   val r4 = mapForAge(source, func)
   println(r4)

   val r5 = compare(5, 10)
   println(r5)

   val isOver30: (Human) => Boolean = (h: Human) => h.age > 30
   val humanList = source.filter(isOver30)
   println(humanList)
   val twiceAge: (Human) => Int = (h: Human) => h.age * 2
   val ages = source.map(twiceAge)
   println(ages)

   def isMale(s: Human) = {
     s match {
       case m: Male => true
       case _ => false
     }
   }

   def filterByAge(s: List[Human]): Int => List[Human] = {
     def f(x: Int): List[Human] = {
       s.filter(_.age > x)
     }
     f
   }

   def mapForAge(l: List[Human], f: (Int) => Int): List[Int] = {
     l.map((s: Human) => f(s.age))
   }

   def compare(x: Int, y: Int): Int = {
     if (x > y)
       x
     else
       y
   }
}

