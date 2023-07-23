package version_2

sealed trait Expr

case class Leaf(value: Boolean) extends Expr

case class And(a: Expr, b: Expr) extends Expr

case class Or(a: Expr, b: Expr) extends Expr

case class Not(a: Expr) extends Expr

case class I(int: Int) extends Expr // we need to support integer as well
case class Sum(left: Expr, right: Expr) extends Expr


object Main {
  // we need to types cast for everything and the programing can be crash anytime
  def intepreter(expr: Expr): Any = {
    expr match {
      case I(i) => i
      case Leaf(a) => a
      case And(a, b) => intepreter(a).asInstanceOf[Boolean] && intepreter(b).asInstanceOf[Boolean]
      case Or(a, b) => intepreter(a).asInstanceOf[Boolean] || intepreter(b).asInstanceOf[Boolean]
      case Not(a) => !intepreter(a).asInstanceOf[Boolean]
      case Sum(left, right) => intepreter(left).asInstanceOf[Int] + intepreter(right).asInstanceOf[Int]
    }
  }

  def main(args: Array[String]): Unit = {
    println(intepreter(Or(And(Leaf(true), Leaf(false)), Leaf(true))))
      println(intepreter(Or(And(I(1), Leaf(false)), Leaf(true)))) // will be crash

  }
}