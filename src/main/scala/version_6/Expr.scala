package version_6

trait Algebras[E[_]] {
  def b(boolean: Boolean): E[Boolean]

  def i(int: Int): E[Int]

  def or(left: E[Boolean], right: E[Boolean]): E[Boolean]

  def and(left: E[Boolean], right: E[Boolean]): E[Boolean]

  def sum(left: E[Int], right: E[Int]): E[Int]
}


object Algebras {
  case class SimpleAlgebras[A](value: A)

  val simpleAlgebras: Algebras[SimpleAlgebras] = new Algebras[SimpleAlgebras] {
    override def b(boolean: Boolean): SimpleAlgebras[Boolean] = SimpleAlgebras(boolean)

    override def i(int: Int): SimpleAlgebras[Int] = SimpleAlgebras(int)

    override def or(left: SimpleAlgebras[Boolean], right: SimpleAlgebras[Boolean]): SimpleAlgebras[Boolean] = SimpleAlgebras(
      left.value || right.value
    )

    override def and(left: SimpleAlgebras[Boolean], right: SimpleAlgebras[Boolean]): SimpleAlgebras[Boolean] =
      SimpleAlgebras(left.value && right.value)

    override def sum(left: SimpleAlgebras[Int], right: SimpleAlgebras[Int]): SimpleAlgebras[Int] = SimpleAlgebras(
      left.value + right.value
    )
  }

  def program[E[_]](implicit algebras: Algebras[E]): E[Boolean] = {
    algebras.or(algebras.b(true), algebras.and(algebras.b(true), algebras.b(false)))

  }

  def program1[E[_]](implicit algebras: Algebras[E]): E[Int] = {
    algebras.sum(algebras.i(1), algebras.i(3))
  }


  def main(args: Array[String]): Unit = {
    println(program[SimpleAlgebras](simpleAlgebras).value)
    println(program[SimpleAlgebras](simpleAlgebras).value)

  }
}