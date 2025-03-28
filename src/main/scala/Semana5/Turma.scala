package Semana5

  import Semana5.RegimeOPT.RegimeOPT
  import Semana5.Turma._
  case class Turma(id: String, alunos: Alunos){
    ???
  }
  object Turma {
    type Nome = String
    type Numero = Int
    type NT = Option[Float]
    type NP = Option[Float]
    type Regime = RegimeOPT
    type Aluno = (Numero, Nome, Regime, NT, NP)
    type Alunos = List[Aluno]
  
    def trabs(t: Turma): Alunos = {
      ???
    }
  }


