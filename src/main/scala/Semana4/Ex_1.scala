package Semana4

object Ex_1 {

  def concatFR[E](a: List[E], b: List[E]): List[E] = {
    (a foldRight b)((x, r) => x :: r) //:: (usado no foldRight) é rápido (O(1)) porque adiciona um elemento na frente da lista.
  }

  def concatFL[E](a: List[E], b: List[E]): List[E] = {
    (b foldLeft a)((r, x) => r :+ x) //:+ (usado no foldLeft) é lento (O(n)) porque precisa copiar toda a lista antes de adicionar o elemento
  }

  def conjunctionFR(a: List[Boolean]): Boolean = {
    (a foldRight true)((x, r) => x && r)
  }

  def conjunctionFL(a: List[Boolean]): Boolean = {
    (a foldLeft true)((x, r) => r && x)
  }

  def disjunctionFR(a: List[Boolean]): Boolean = {
    (a foldRight false)((x, r) => x || r)
  }

  def disjunctionFL(a: List[Boolean]): Boolean = {
    (a foldLeft false)((r, x) => r || x)
  }

  def remDumpFR(a: List[Char]): List[Char] = {
    (a foldRight List[Char]())((x, r) => x :: r.dropWhile((y: Char) => {
      y == x
    }))
  }

  type Team = String
  type Goals = Int
  type Match = ((Team, Goals), (Team, Goals))
  type Fixtures = List[Match]

  def notItSelfFR(a: Fixtures): Boolean = {
    (a foldRight true)((x, r) => x._1._1 != x._2._1 && r)
  }

  def notItSelfFL(a: Fixtures): Boolean = {
    (a foldLeft true)((r, x) => r && (x._1._1 != x._2._1))
  }

  def teamsFR(a: Fixtures): List[Team] = {
    (a foldRight List[Team]())((x, r) =>
      if (r.contains(x._1._1) && r.contains(x._2._1))
        r
      else if (r.contains(x._1._1))
        x._2._1 :: r
      else if (r.contains(x._2._1))
        x._1._1 :: r
      else x._1._1 :: x._2._1 :: r
    ).reverse
  }

  def teamsFL(a: Fixtures): List[Team] = {
    (a foldLeft List[Team]())((r, x) =>
      if (r.contains(x._1._1) && r.contains(x._2._1))
        r
      else if (r.contains(x._1._1))
        r :+ x._2._1
      else if (r.contains(x._2._1))
        r :+ x._1._1
      else r :+ x._1._1 :+ x._2._1
    )
  }

  def drawsFR(a: Fixtures): List[(Team, Team)] = {
    (a foldRight List[(Team, Team)]())((x, r) =>
      if (x._1._2 == x._2._2)
        (x._1._1, x._2._1) :: r
      else r
    )
  }

  def drawsFL(a: Fixtures): List[(Team, Team)] = {
    (a foldLeft List[(Team, Team)]())((r, x) =>
      if (x._1._2 == x._2._2)
        r :+ (x._1._1, x._2._1)
      else r
    )
  }

  def pointsFR(a: Fixtures): List[(Team, Int)] = {
    (a foldRight List[(Team, Int)]())((x, r) =>
      if (x._1._2 > x._2._2)
        (x._1._1, 3) :: (x._2._1, 0) :: r
      else if (x._1._2 < x._2._2)
        (x._1._1, 0) :: (x._2._1, 3) :: r
      else (x._1._1, 1) :: (x._2._1, 1) :: r

    )
  }

  def main(args: Array[String]): Unit = {
    val fixtures: Fixtures = List(
      (("TeamA", 2), ("TeamB", 1)), // TeamA vence (3 pts)
      (("TeamC", 1), ("TeamD", 3)), // TeamD vence (3 pts)
      (("TeamE", 0), ("TeamF", 0)), // Empate (1 pt cada)
      (("TeamA", 1), ("TeamC", 1)) // Empate (1 pt cada)
    )

    println(pointsFR(fixtures))
    // Esperado: List((TeamA, 4), (TeamB, 0), (TeamC, 1), (TeamD, 3), (TeamE, 1), (TeamF, 1))
  }
}
