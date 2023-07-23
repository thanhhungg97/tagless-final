package version_5

sealed trait Expr[A] {
  val value: A // final value we care abount

}
object Expr{
  def b(boolean: Boolean): Expr[Boolean] = new Expr[Boolean] {
    override val value: Boolean = boolean
  }

  def and(a: Expr[Boolean], b: Expr[Boolean]) = new Expr[Boolean] {
    override val value: Boolean = a.value && b.value
  }

  def or(a: Expr[Boolean], b: Expr[Boolean]) = new Expr[Boolean] {
    override val value: Boolean = a.value || b.value
  }

  def not(a: Expr[Boolean]) = new Expr[Boolean] {
    override val value: Boolean = !a.value
  }

  def i(int: Int) = new Expr[Int] {
    override val value: Int = int
  }

  def sum(a: Expr[Int], b: Expr[Int]) = new Expr[Int] {
    override val value: Int = a.value + b.value
  }
  def eval[A](expr: Expr[A]): A = expr.value

}


object Main{
  import Expr._
  def main(args: Array[String]): Unit = {
    println(eval(or(b(true), and(b(true), b(false)))))
    println(eval(sum(i(24), i(-3))))
  }

}