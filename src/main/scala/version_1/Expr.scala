package version_1

sealed trait Expr

case class Leaf(value: Boolean) extends Expr

case class And(a: Expr, b: Expr) extends Expr

case class Or(a: Expr, b: Expr) extends Expr

case class Not(a: Expr) extends Expr

object Main {
  def intepreter(expr: Expr): Boolean = {
    expr match {
      case Leaf(a) => a
      case And(a, b) => intepreter(a) && intepreter(b)
      case Or(a, b) => intepreter(a) || intepreter(b)
      case Not(a) => !intepreter(a)

    }
  }

  def main(args: Array[String]): Unit = {
    println(intepreter(Or(And(Leaf(true), Leaf(false)), Leaf(true)))
    )
  }
}