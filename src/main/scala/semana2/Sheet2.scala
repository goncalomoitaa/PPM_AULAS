package semana2

object Sheet2 {
  def ex1(x: Int) = {
    println("TODO")
  }

  def main(args: Array[String]): Unit = {
    val a = List(3,4,5,5)
    val b = List(1.0,3.0,4.0)
    val c = List((1,2), (3,4), (8,1), (8,8), (1,5), (8, 9))
    val d = List(
      ("Alice", "212345678", "alice@example.com"),
      ("Bob", "312345678", "bob@example.com"),
      ("Charlie", "212987654", "charlie@example.com"),
      ("David", "212345678", "david@example.com")
    )
//    ex1(1)
//    print(transf(a))
//    print(product(a))
//    print(addElement(a, 4))
//    print(concatenate(a, b))
//    print(sumEl(c, 0))
//    print(lengthAndSum(b))
//    print(average(b))
//    print(splitByValue(b, 4))
//    print(splitByAverage(b))
//    print(filterEmailsByPrefix(d))
//    print(filterByName(d, ""))
  }

  def transf[E](a : List[E]): List[E] = a match {
    case Nil => Nil
    case primeiro :: Nil => a
    case primeiro :: segundo :: tail => segundo :: primeiro :: transf(tail)
  }

  def product(a : List[Int]): Int = a match {
    case Nil => 0
    case primeiro :: Nil => primeiro
    case primeiro :: tail => primeiro * product(tail)
  }

  def addElement[E](a : List[E], b : E): List[E] = a match {
    case Nil => List(b) //vamos esvaziando a lista para depois no final ser possivel de adicionar um
    case primeiro :: tail => primeiro :: addElement(tail, b)
  }

  def concatenate[E](a : List[E], b : List[E]): List[E] = a match {
    case Nil => b
    case primeiro :: Nil => primeiro :: b
    case primeiro :: tail => primeiro :: concatenate(tail, b)
  }

  def sumEl(a: List[(Int, Int)], b : Int): Int = (a, b) match {
    case (Nil, 0) => 0
    case (primeiro :: Nil, 2) => primeiro._1 + primeiro._2
    case (primeiro :: tail, 2) => primeiro._1 + primeiro._2 + sumEl(tail, b + 1)
    case (primeiro :: Nil, 4) => primeiro._1 + primeiro._2
    case (primeiro :: tail, 4) => primeiro._1 + primeiro._2
    case (_ :: tail, _) => sumEl(tail, b + 1)
  }

  def lengthAndSum(a : List[Double]): (Int, Double) = a match {
    case Nil => (0,0.0)
    case primeiro :: Nil => (1, primeiro)
    case primeiro :: tail => (1 + lengthAndSum(tail)._1, primeiro + lengthAndSum(tail)._2)
  }

  def average(a : List[Double]): Double = {
    lengthAndSum(a)._2 / lengthAndSum(a)._1
  }

  def splitByValue(a : List[Double], b : Double): (List[Double], List[Double]) = a match {
    case Nil => (Nil, Nil)
    case primeiro :: tail if(primeiro <= b) => (primeiro :: splitByValue(tail, b)._1, splitByValue(tail, b)._2)
    case primeiro :: tail if(primeiro > b) => (splitByValue(tail, b)._1, primeiro :: splitByValue(tail, b)._2)
  }

  def splitByAverage(a : List[Double]): (List[Double], List[Double]) = a match {
    case Nil => (Nil, Nil)
    case primeiro :: tail => val b = average(a)
    if(primeiro < b) (primeiro :: splitByAverage(tail)._1, splitByAverage(tail)._2)
    else
      (splitByAverage(tail)._1, primeiro :: splitByAverage(tail)._2)
  }

  type Entry = (String, String, String)
  type LTelef = List[Entry]

  def emails(lst: LTelef): List[String] = {
    lst match {
      case Nil => Nil
      case (_, _, email) :: tail => email :: (emails(tail))
    }
  }

  def filterEmailsByPrefix(a : LTelef): List[String] = a match {
    case Nil => Nil
    case (_, numero, email) :: tail if(numero.charAt(0).equals('2')) => email :: filterEmailsByPrefix(tail)
    case (_, numero, email) :: tail if(!numero.charAt(0).equals('2')) => filterEmailsByPrefix(tail)
  }

  def filterByName(a : LTelef, b : String): List[(String, String)] = a match {
    case Nil => Nil
    case (nome, numero, email) :: tail if(nome.equals(b)) => (numero , email) :: filterByName(tail, b)
    case (nome, numero, email) :: tail if(!nome.equals(b)) => filterByName(tail, b)
  }

}
