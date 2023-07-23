package version_4


trait Expr[+A] // using generic type argument => tagless initial -> an intermediate data structures, not the value we care about
case class B(boolean: Boolean) extends Expr[Boolean]
case class Or(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean]
case class And(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean]
case class Not(expr: Expr[Boolean]) extends Expr[Boolean]
case class I(int: Int) extends Expr[Int]
case class Sum(left: Expr[Int], right: Expr[Int]) extends Expr[Int]


object main{
  def eval[A](expr: Expr[A]): A  = expr match {
    case B(b) => b
    case I(i) => i
    case Or(left, right) => eval(left) || eval(right)
    case Sum(left, right) => eval(left) + eval(right)
    case And(left, right) => eval(left) && eval(right)
    case Not(expr) => !eval(expr)
  }

  def main(args: Array[String]): Unit = {
    println(eval(Or(And(B(true), B(false)), B(true))))
   // println(eval(Or(And(I(1), B(false)), B(true)))) // check at compile time, the program cannot be crash

  }
}