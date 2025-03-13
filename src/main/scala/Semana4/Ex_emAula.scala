package Semana4

object Ex_emAula {

  def concat(l1 : List[Int], l2 : List[Int]): List[Int] = (l1, l2) match {
    case (Nil, y) => y
    case (x :: xs, y) => x :: concat(xs, y)
  }

//  concat(List(1,2), List(3,4))
//  1 :: concat(List(1,2), List(3,4))
//        2 :: concat(Nil, List(3,4))
//              List(3,4)
//        2 :: List(3,4)
//  1 :: 2 :: List(3,4)

  def concatFR(l1 : List[Int], l2 : List[Int]): List[Int] = { //usando agora foldRigh
    (l1 foldRight l2)((x, r) => x :: r)  //x reprsenta o primeiro elemento da lista e o r é a evocação recursiva sobre o resto lista
  }

  def concatFL(l1 : List[Int], l2 : List[Int]): List[Int] = { //começa do final até ao início usando foldLeft e o foldRight ao contrario
    (l2 foldLeft l1)((r, x) => r :+ x)  //invocação recursiva de todos os elementos menos o último e o do foldRight é ao contrário
  }

  def mapFunFR[T, U](l : List[T], f : T => U): List[U] = {
    (l foldRight  List[U]())((x, r) => f(x) :: r)
  }

  def mapFunFL[T, U](l: List[T], f: T => U): List[U] = {
    (l foldLeft List[U]())((r, x) => r :+ f(x))
  }

  def lengthFunFR[T](l: List[T]): Int = {
    (l foldRight 0)((x, r) => 1 + r)
  }

  def lengthFunFL[T](l: List[T]): Int = {
    (l foldLeft 0)((r, x) => 1 + r)
  }

//  def lengthAndSumFunFR[T](l: List[T]): (Int, Int) = {
//    (l foldRight 0)((x, r) => (1 + r._1, x + r._2)
//  } exercicio inventado pelo  stor que retorna (Tamanho, somatório)

//implicit é passar a funcao sem usar o parametro   

}
