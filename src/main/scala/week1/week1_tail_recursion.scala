package week1

object week1_tail_recursion extends App{
  // compute the Greatest Common Divisor of two numbers
  // implementation using Euclid's algorithm
  // this is a tail recursive method b/c gcd is last f(x) called
  def gcd(a:Int, b:Int): Int = {
    if (b==0) a else gcd(b, a % b)
  }

  // compute a factorial calculation
  // this is NOT tail recursive b/c a multiplication happens at the end
  def factorial(n: Int): Int = {
    if (n == 0) 1 else n * factorial(n-1)
  }

  // tail recursive version of factorial calculation
  def factorial_tr(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)
    loop(1, n)
  }

  println(factorial_tr(4))
}
