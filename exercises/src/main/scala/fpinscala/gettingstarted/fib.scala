object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def assertEqual[A,B](lhs:A, rhs:B, msg: String ="") = {
    if (lhs == rhs) ()
    else {
      val premsg = if(msg == "") "Assertion Failed! in" else msg
      println(premsg + " " + lhs + " != " + rhs)
    }
  }

  def main(args: Array[String]): Unit = {
    val fibo = Array(-1, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811)
    def loop(i: Int, max: Int): Unit = {
      if (i == max) ()
      else {
        print(i + ": ")
        assertEqual(fib(i), fibo(i))
        loop(i + 1, max)
      }
    }  
    loop(1, fibo.length)
  }

  // Exercise 1: Write a function to compute the nth fibonacci number
  def fib(n: Int): Int = {
    def loop(a: Int, b: Int, i: Int): Int = {
      if (n == 1) 0
      else if (i == n) a
      else loop(b, a + b, i + 1)
    }
    loop(0,1, 1)
  }

  // A definition of factorial, using a local, tail recursive function
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  // Another implementation of `factorial`, this time with a `while` loop
  def factorial2(n: Int): Int = {
    var acc = 1
    var i = n
    while (i > 0) { acc *= i; i -= 1 }
    acc
  }

}
