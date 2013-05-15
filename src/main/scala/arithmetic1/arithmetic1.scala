package arithmetic1

class S99Int(val start: Int) {

}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
}