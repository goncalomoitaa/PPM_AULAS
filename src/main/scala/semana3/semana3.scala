package semana3

import scala.annotation.tailrec

object semana3 {

  /////////////////////////////////////////////////EXERCICIO 1.1////////////////////////////////////////////////////////

  def factorial1(a : Int): Int = a match {
    case 0 => 1
    case _ => a * factorial1(a - 1)
  }

  def factorial2(a: Int): Int = a match {
    case n => if(n > 0) {
      n * factorial2(n - 1)
    } else {
      1
    }
  }

  def factorial3(n : Int): Int = {
    @tailrec
    def loop(acc : Int, n : Int): Int = {
      if(n == 0)
        acc
      else loop(acc * n, n - 1)
    }
    loop(1, n)
  }

  /////////////////////////////////////////////////EXERCICIO 1.2////////////////////////////////////////////////////////

  def remDup_a(a : List[Char]): List[Char] = a match {
    case Nil => Nil
    case primeiro :: Nil => List(primeiro)
    case primeiro :: segundo :: tail  if(primeiro == segundo) => remDup_a(segundo :: tail)
    case primeiro :: segundo :: tail if(primeiro != segundo) => primeiro :: remDup_a(segundo :: tail)
  }

  def remDup_b(a : List[Char]): List[Char] = {
    @tailrec
    def loop(a : List[Char], acc : List[Char]): List[Char] = a match {
      case Nil => acc.reverse
      case primeiro :: Nil => acc.reverse
      case primeiro :: segundo :: tail if(primeiro == segundo) => loop(tail, segundo :: acc)
      case primeiro :: tail  => loop(tail, primeiro :: acc)
    }
    loop(a, Nil)
  }

  /////////////////////////////////////////////////EXERCICIO 2////////////////////////////////////////////////////////

  def lazyListRange(lo: Int, hi: Int): LazyList[Int] = { //lazyListRange(1, 100) cria um LazyList, mas nenhum elemento é gerado
    println(lo)                                          // imediatamente, quando é chamado o take ai sim
    if (lo >= hi) LazyList.empty
    else lo #:: lazyListRange(lo + 1, hi)
  }

  def listRange(lo: Int, hi: Int): List[Int] = { //ao chamar listRange(1, 100), toda a lista de 1 a 99 é gerada antes de chamar o take
    println(lo)
    if (lo >= hi) List()
    else lo :: listRange(lo + 1, hi)
  }

  /////////////////////////////////////////////////EXERCICIO 3////////////////////////////////////////////////////////

  def sumLists(a: List[Int], b: List[Int]): List[Int] = {
    @tailrec
    def loop(a: List[Int], b: List[Int], acc : List[Int]): List[Int] = (a, b) match {
      case (Nil, Nil) => acc.reverse
      case (Nil, primeiro_b :: tail) => loop(Nil, tail, primeiro_b :: acc)
      case (primeiro_a :: tail, Nil) => loop(tail, Nil, primeiro_a :: acc)
      case (primeiro_a :: tail_a, primeiro_b :: tail_b) => loop(tail_a, tail_b, (primeiro_a + primeiro_b) :: acc)
    }
    loop(a, b, Nil)
  }

  def zipWith[E](f: (E, E) => E, a: List[E], b: List[E]): List[E] = {
    @tailrec
    def loop(a: List[E], b: List[E], acc: List[E]): List[E] = (a, b) match {
      case (Nil, Nil) => acc.reverse
      case (Nil, primeiro_b :: tail) => loop(Nil, tail, primeiro_b :: acc)
      case (primeiro_a :: tail, Nil) => loop(tail, Nil, primeiro_a :: acc)
      case (primeiro_a :: tail_a, primeiro_b :: tail_b) => loop(tail_a, tail_b, f(primeiro_a, primeiro_b) :: acc)
    }
    loop(a, b, Nil)
  }

  def isSorted[A](lst: List[A], ordered: (A,A) => Boolean): Boolean = lst match {
    case Nil => true
    case primeiro :: Nil => true
    case primeiro :: segundo :: tail => ordered(primeiro, segundo) && isSorted(segundo :: tail, ordered)
  }

  def bubbleSort(data: List[Int], f: (Int, Int) => Boolean): List[Int] = {
    def loop(data : List[Int], f : (Int, Int) => Boolean): List[Int] = data match {
      case Nil => Nil
      case primeiro :: Nil => List(primeiro)
      case primeiro :: segundo :: tail => val bool = f(primeiro, segundo)
        if(bool) segundo :: loop(primeiro :: tail, f)
        else
          primeiro :: loop(segundo :: tail, f)
    }
    if (data.length <= 1) data
    else {
      val sortedPass = loop(data, f) // Faz uma passagem do Bubble Sort
      bubbleSort(sortedPass.init, f) :+ sortedPass.last // Recorre sem o último elemento
    }
  }

  /////////////////////////////////////////////////EXERCICIO 4////////////////////////////////////////////////////////

  def paresOrd(a : List[(Int, Int)]): List[(Int, Int)] = a match { //é usado o filtering
    case Nil => Nil
    case (primeiro, segundo) :: tail =>
      if(primeiro < segundo) (primeiro, segundo) :: paresOrd(tail)
      else paresOrd(tail)
  }

  def myConcat(a : List[String]): String = a match { //é usado folding
    case Nil => ""
    case primeiro :: tail => primeiro + myConcat(tail)
  }

  def maximum(a : List[(Double, Double)]): List[Double] = a match { //é usado mapping
    case Nil => Nil
    case (primeiro, segundo) :: tail =>
      if(primeiro > segundo) primeiro :: maximum(tail)
      else segundo :: maximum(tail)
  }

  ///////////////////////////////////////////////EXERCICIO 5////////////////////////////////////////////////////////

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3, 4, 5) map (x => x % 2 != 0)) //testa cada um dos elementos da lista para ver se são pares ou impares, e da false ou true
    println(List(1, 2, 3, 4, 5) filter (x => x % 2 != 0)) //mostra todos os elementos da lista que são impares
    println(List(5, 6, 23, 3) map (x => x % 3 == 0)) //da true se for divisível por 3 e false c.c
    println(List(1, 3, 7, 8, 12, 15) filter (x => x < 7)) //mostra todos os elementos que são menores que 7
    println(List(List(2, 3), List(1, 5, 3)) map (x => 7 :: x)) //adiciona um 7 ao primeiro elemento(lista 1,2,3) e outro 7 ao segundo elemento
    println(List(1, 2, 3, 4, 5) map (x => List(x))) //uma lista os mesmo elementos, mas agora cada uma faz parte de uma lista diferente
    println(List(1,2,3,4,5) filter ( x => x%2 !=0) map ( x => x + 1)) //vai somar 1 a todos os elementos impares da lista
    println(List(1,2,3,4,5) map ( x => x + 1) filter ( x => x%2 !=0)) //vai somar 1 a todos os elementos da lista e depois só ficam os que se tornaram impares
  }

  ///////////////////////////////////////////////EXERCICIO 6////////////////////////////////////////////////////////

  def indicative(ind: String, telefs: List[String]): List[String] = telefs match {
    case Nil => Nil
    case primeiro :: tail => if (primeiro.substring(0, ind.length).equals(ind)) primeiro :: indicative(ind, tail)
    else indicative(ind, tail)
  }

  ///////////////////////////////////////////////EXERCICIO 7////////////////////////////////////////////////////////

  def abbreviateNames(a: List[String]): List[String] = {
    a.map { name =>
      val firstName = name.split(" ").head.substring(0,1)
      val lastName = name.split(" ").last
      s"$firstName. $lastName"
    }
  }

}
