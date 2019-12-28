object PolymorphicFunctions {
  def testIsSorted(): Unit = {
    val sortedInts = Array(1,2,3,4,5)
    val randomInts = Array(4,1,2,6,10,3,5)
    val randomInts2 = Array(1,2,3,4,5,1)
    val sortedStrs = Array("a","bes","ccd","d")
    val randomStrs = Array("bxc","a","c","dlw")
    val randomStrs2 = Array("a","bes","ccd","x","d")
    
    //val aa = Array(sortedInts, sortedStrs, randomInts, randomStrs) 
    //println(aa.map((arr) => arr.mkString(" ")))
    
    assertEqual(isSorted(sortedInts, (a:Int, b:Int) => a > b), true)
    assertEqual(isSorted(randomInts, (a:Int, b:Int) => a > b), false)
    assertEqual(isSorted(randomInts2,(a:Int, b:Int) => a > b), false)
    assertEqual(isSorted(sortedStrs, (a:String, b:String) => a > b), true)
    assertEqual(isSorted(randomStrs, (a:String, b:String) => a > b), false)
    assertEqual(isSorted(randomStrs2,(a:String, b:String) => a > b), false)
  }

  def main(args: Array[String]): Unit = {
    testIsSorted()
    //val sortedInts = Array(1,2,3,4,5)
    //val s = sortedInts.sliding(2)
  }

  private def assertEqual[A,B](lhs:A, rhs:B, msg: String ="") = {
    if (lhs == rhs) ()
    else {
      val premsg = if(msg == "") "Assertion Failed! in" else msg
      println(premsg + " " + lhs + " != " + rhs)
    }
  }

  // Here's a polymorphic version of `binarySearch`, parameterized on
  // a function for testing whether an `A` is greater than another `A`.
  def binarySearch[A](as: Array[A], key: A, gt: (A,A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key,a)) mid2
        else if (greater) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    def loop(i:Int, ret:Boolean):Boolean = {
      if (i == as.length || ret == false) ret
      else if (gt(as(i - 1), as(i))) loop(-1, false)
      else loop(i + 1, true)
    }
    loop(1, true)
  }

  // Polymorphic functions are often so constrained by their type
  // that they only have one implementation! Here's an example:

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise 3: Implement `curry`.

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    ???

  // NB: The `Function2` trait has a `curried` method already

  // Exercise 4: Implement `uncurry`
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    ???

  /*
  NB: There is a method on the `Function` object in the standard library,
  `Function.uncurried` that you can use for uncurrying.

  Note that we can go back and forth between the two forms. We can curry
  and uncurry and the two forms are in some sense "the same". In FP jargon,
  we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
  a term we inherit from category theory.
  */

  // Exercise 5: Implement `compose`

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    ???
}
