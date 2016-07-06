import Math.{ pow }
import HumanImports._

abstract class Human {
  def age: Int
  def height: Double
  def weight: Double
  def bmi: Double = weight / pow(height / 100, 2)
}

case class Male(val age: Int, val height: Double, val weight: Double) extends Human

case class Female(val age: Int, val height: Double, val weight: Double) extends Human

object Statistics extends App {

  // 1. Function is first-class object
  val func: (Int) => Int = (x :Int) => x * 2;
  //val func = (x :Int) => x * 2;
  println(func(2))

   // Input source
   val maleList: List[Male] = List(Male(31, 176.0, 63.0), Male(19, 170, 60.0), Male(21, 168.0, 58.0), Male(16, 172.0, 65.0), Male(32, 165.0, 60.0))
   val femaleList: List[Female] = List(Female(18, 154.0, 52.0), Female(28, 166, 58.0), Female(23, 158.0, 56.0), Female(32, 168.0, 60.0), Female(17, 148.0, 48.0))
   val source: List[Human] = maleList ::: femaleList

   println("---Youngers---")
   val isYoung: (Human) => Boolean = (h: Human) => h.isYoung
   val r: List[Human] = source.filter(isYoung)
   println(r)
   println("---Males---")
   val r2: List[Human] = source.filter((s: Human) => isMale(s))
   println(r2)
   println("---Young Sorted Age Males---")
   // val ageList: List[Int] = male.filter(isYoung).map(_.age).sortWith(_>_)
   // val ageList: List[Int] = maleList.filter(isYoung).map(_.age).sorted
   val ageList: List[Int] = maleList.filter(_.isYoung).map(_.age).sorted
   println(ageList)
   println("---Not Young Reverse Sorted Height Females---")
   // val heightList: List[Double] = femaleList.filterNot(isYoung).map(_.height).sorted.reverse
   val heightList: List[Double] = femaleList.filterNot(_.isYoung).map(_.height).sorted.reverse
   println(heightList)

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

