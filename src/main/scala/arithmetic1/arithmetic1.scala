package arithmetic1

class S99Int(val start: Int) extends AnyVal {

  import S99Int._

  def isPrime: Boolean = start == 2 || start >= 3 &&
      (2 until start).forall(i => !i.isPrime || start % i != 0)

}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
}