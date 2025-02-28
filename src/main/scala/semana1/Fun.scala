package semana1

//singleton object
object Fun {

  def func1(x : Double, y :Int) = x + (70*y)

  def ex(a: Double) = 50 * a

  def ex2_a(x: (Int, Int), y: (Int, Int)) = {
    val a = x._1 + x._2
    val b = y._1 * y._2
    (a, b)
  }

  def ex2_b(a : Int, b : Int, c : Int) = {
    val lista = List(a, b, c)
    val sortedList = lista.sorted
    (sortedList(2), sortedList(1))
  }

  def ex2_c(a : Int, b : Int, c : Int) = {
    val lista = List(a, b, c)
    val sortedList = lista.sorted.reverse
    (sortedList(0), sortedList(1), sortedList(2))
  }

  def ex2_d(a : Int, b : Int, c : Int) = {
    if(a + b > c && a + c > b && b + c > a)
      true
    else
      false
  }

  def ex2_e(a : String) = {
    val b = a.split(" ")
    if(!b.isEmpty)
      if(b.length > 1)
        (b.head, b.last)
      else
        b.head
    else
      "String está vazia"
  }

  def ex3_a(x : Int, n : Int): Int = {
    if(n == 0)
      1
    else
      x * ex3_a(x, n - 1)
  }

  def ex3_b(a : List[Int]): (Int, Int) = {
    if(a.length > 1)
      (a.head, ex3_b(a.tail)._2) //se quisessemos o primeiro só usavamos init
    else
      if(a.length == 1)
        (a.head, a.head)
      else
        null
  }

  def ex3_c(a: List[Int]): (List[Int], Int) = {
    val b = a
    (b, b.indexOf(ex3_b(a)._2) - b.indexOf(ex3_b(a)._1) + 1)
  }

  def ex3_d(a: List[Double]): Double = {
    sum(a) / a.length
  }

  def sum(a: List[Double]): Double = {
    if(a.length >= 1)
      a.head + sum(a.tail)
    else
      0
  }

}