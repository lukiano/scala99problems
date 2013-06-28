import arithmetic1.S99Int

class ArithmeticProblems extends org.scalatest.FunSuite {

  import S99Int._

  test("Problem 31: Determine whether a given integer number is prime.") {
    assert(7.isPrime)
  }

  test("Problem 32: Determine the greatest common divisor of two positive integer numbers."){
    def gcd(first: Int, second: Int): Int = if (first == second) first
      else if (first < second) gcd(first, second - first)
      else gcd(first - second, second)
    assert(gcd(36, 63) === 9)
  }

}
