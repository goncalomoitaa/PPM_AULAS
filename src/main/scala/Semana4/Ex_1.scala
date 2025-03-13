package Semana4

object Ex_1 {

  def concatFR[E](a : List[E], b : List[E]): List[E] = {
    (a foldRight b)((x, r) => x :: r) //case a = nil , e b tem algo retorna b  case (Nil, y) => y
  }

  def concatFL[E](a : List[E], b : List[E]): List[E] = {
    (b foldLeft a)((r, x) => r :+ x)
  }

  def conjunctionFR(a : List[Boolean]): Boolean = {
    (a foldRight true)((x, r) => x && r)
  }

  def conjunctionFL(a : List[Boolean]): Boolean = {
    (a foldLeft true)((r, x) => r && x)
  }

  def disjunctionFR(a: List[Boolean]): Boolean = {
    (a foldRight false)((x, r) => x || r)
  }

  def disjunctionFL(a: List[Boolean]): Boolean = {
    (a foldLeft false)((r, x) => r || x)
  }

  def remDump(a : List[Char]): List[Char] = {
    (a foldRight List[Char]())((x, r) =>
      if(r.nonEmpty && x == r.head)//ver depois 
        r
      else
        x :: r
    )
  }

  def main(args: Array[String]): Unit = {
    val lista1 = List('a','a','a','a','b','c','d','t')
    val lista2 = List(4, 5, 6)
    val esperado = List(1, 2, 3, 4, 5, 6)
    val boolList1 = List(false, false, false)
//    print(concatFR(lista1, lista2))
//    print(concatFL(lista1, lista2))
//    print(conjunctionFR(boolList1))
//    print(conjunctionFL(boolList1))
    print(disjunctionFR(boolList1))
    print(disjunctionFL(boolList1))
    print(remDump(lista1))
  }

}
