package Semana5

  //Example.scala (Case Class and Companion Object)
  sealed trait MyTree[+A] //sealed diz que todos os tipos de mapa são conhecidos

  case object Empty extends MyTree[Nothing] //mapa vazio

  case class Node[A](value: A, left: MyTree[A], right: MyTree[A]) extends MyTree[A] //Este é um mapa com um tesouro (value) e dois outros mapas (left e right) que podem conter mais tesouros.

  case class Example[A](myField: MyTree[A]) { //Example é um explorador que tem um mapa (myField) para explorar, pode ser um dos casos anteriores
    def maximum() = Example.maximum(this.myField.asInstanceOf[MyTree[Int]])

    def depth() = Example.depth(this.myField)

    def map[B](f: A => B): MyTree[B] = Example.map(this.myField)(f)
  }

  object Example {
    def maximum(t: MyTree[Int]): Option[Int] = t match {
      case Empty => None
      case Node(value, left, right) =>
        val leftMax = maximum(left)
        val rightMax = maximum(right)
        val currentMax = leftMax match {
          case None => value
          case Some(l) => rightMax match {
            case None => if (l > value) l else value
            case Some(r) => if (l > value && l > r) l else if (r > value) r else value
          }
        }
        Some(currentMax)
    }

    def depth[A](t: MyTree[A]): Int = t match {
      case Empty => 0
      case Node(_, left, right) =>
        val leftDepth = depth(left)
        val rightDepth = depth(right)
        if (leftDepth > rightDepth) 1 + leftDepth else 1 + rightDepth

    }

    def map[A, B](t: MyTree[A])(f: A => B): MyTree[B] = t match {
      case Empty => Empty
      case Node(value, left, right) => Node(f(value), map(left)(f), map(right)(f))

    }

    def main(args: Array[String]): Unit = {
      val tree1 = Node(42, Node(8, Empty, Empty), Empty)
      val t = Example(tree1)
      println(s"Maximum element of the tree: ${t.maximum()}")
      println(s"Depth of the tree: ${t.depth()}")
      println(s"Map: ${t.map((x: Int) => x * 2)}")
    }
  }

