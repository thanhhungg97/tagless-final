package version_3


sealed trait Expr

sealed trait IntExpr extends Expr

sealed trait BooleanExpr extends Expr

// we can produce new layer for Int express and boolean express, but we also need to type cast


case class B(boolean: Boolean) extends BooleanExpr

case class Or(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr

case class And(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr

case class Not(expr: BooleanExpr) extends BooleanExpr

case class I(int: Int) extends IntExpr

case class Sum(left: IntExpr, right: IntExpr) extends IntExpr


object Main {
  // we need to types cast for everything and the programing can be crash anytime
  def intepreter(expr: Expr): Any = {
    expr match {
      case I(i) => i
      case B(a) => a
      case And(a, b) => intepreter(a).asInstanceOf[Boolean] && intepreter(b).asInstanceOf[Boolean]
      case Or(a, b) => intepreter(a).asInstanceOf[Boolean] || intepreter(b).asInstanceOf[Boolean]
      case Not(a) => !intepreter(a).asInstanceOf[Boolean]
      case Sum(left, right) => intepreter(left).asInstanceOf[Int] + intepreter(right).asInstanceOf[Int]
    }
  }

  def main(args: Array[String]): Unit = {
    println(intepreter(Or(And(B(true), B(false)), B(true))))
    // println(intepreter(Or(And(I(1), B(false)), B(true)))) // check at compile time, the program cannot be crash

  }
}