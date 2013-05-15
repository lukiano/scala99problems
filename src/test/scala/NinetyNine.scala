import scala.annotation.tailrec
import scala.util.Random

/**
 * http://aperiodic.net/phil/scala/s-99/
 */
class NinetyNine extends org.scalatest.FunSuite {

  //Lists

  test("Problem 1: Find the last element of a list.") {
    def last(list: List[Any]) = list.last
    assert(last(List(1, 1, 2, 3, 5, 8)) === 8)
  }

  test("Problem 2: Find the last but one element of a list.") {
    def penultimate(list: List[Any]): Any = list match {
      case List(a,b) => a
      case c :: d => penultimate(d)
    }
    assert(penultimate(List(1, 1, 2, 3, 5, 8)) === 5)
  }

  test("Problem 3: Find the Kth element of a list.") {
    def nth(n: Int, list: List[Any]) = list(n)
    assert(nth(2, List(1, 1, 2, 3, 5, 8)) === 2)
  }

  test("Problem 4: Find the number of elements of a list.") {
    def length(list: List[Any]) = list.length
    assert(length(List(1, 1, 2, 3, 5, 8)) === 6)
  }

  test("Problem 5: Reverse a list") {
    def reverse(list: List[Any]) = list.reverse
    assert(reverse(List(1, 1, 2, 3, 5, 8)) === List(8, 5, 3, 2, 1, 1))
  }

  test("Problem 6: Find out whether a list is a palindrome") {
    def isPalindrome(list: List[Any]) = list == list.reverse
    assert(isPalindrome(List(1, 2, 3, 2, 1)))
  }

  test("Problem 7: Flatten a nested list structure") {
    def f(any: Any): List[Any] = any match {
      case Nil => Nil
      case x :: xs => flatten(List(x) ++ flatten(xs))
      case z => List(z)
    }
    def flatten(list: List[Any]) = list.flatMap(f)
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) === List(1, 1, 2, 3, 5, 8))
  }

  test("Problem 8: Eliminate consecutive duplicates of list elements.") {
    def compress(list: List[Any]): List[Any] = list match {
      case Nil => Nil
      case List(x) => List(x)
      case x :: y :: xs => if (x==y) compress(y::xs) else x :: compress(y::xs)
    }
    assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("Problem 9: Pack consecutive duplicates of list elements into sublists.") {
    def pack(list: List[Any], times: Int = 1): List[List[Any]] = list match {
      case Nil => Nil
      case List(x) => List(List.fill(times)(x))
      case x :: y :: xs => if (x==y) pack(y::xs, times+1) else List.fill(times)(x) :: pack(y::xs)
    }
    assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("Problem 10: Run-length encoding of a list.") {
    def pack(list: List[Any], times: Int = 1): List[List[Any]] = list match {
      case Nil => Nil
      case List(x) => List(List.fill(times)(x))
      case x :: y :: xs => if (x==y) pack(y::xs, times+1) else List.fill(times)(x) :: pack(y::xs)
    }
    def encode(list: List[Any]) = pack(list).map( list => (list.length, list.head))
    assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  test("Problem 11: Modified run-length encoding.") {
    def pack(list: List[Any], times: Int = 1): List[List[Any]] = list match {
      case Nil => Nil
      case List(x) => List(List.fill(times)(x))
      case x :: y :: xs => if (x==y) pack(y::xs, times+1) else List.fill(times)(x) :: pack(y::xs)
    }
    def f(list: List[Any]) = list match {
      case List(x) => x
      case l => (l.length, l.head)
    }
    def encodeModified(list: List[Any]) = pack(list).map(f)
    assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }

  test("Problem 12: Decode a run-length encoded list.") {
    def decode(list: List[(Int, Symbol)]) = list.flatMap( tuple => List.fill(tuple._1)(tuple._2))
    assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) === List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  test("Problem 13: Run-length encoding of a list (direct solution).") {
    def encodeDirect(list: List[Any], times: Int = 1): List[(Int, Any)] = list match {
      case List(x) => List((times, x))
      case x :: y :: xs => if (x==y) encodeDirect(y::xs, times+1) else (times, x) :: encodeDirect(y::xs)
    }
    assert(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  test("Problem 14: Duplicate the elements of a list.") {
    def duplicate(list: List[Any]) = list.flatMap( any => List.fill(2)(any))
    assert(duplicate(List('a, 'b, 'c, 'c, 'd)) === List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  test("Problem 15:  Duplicate the elements of a list a given number of times.") {
    def duplicateN(times: Int, list: List[Any]) = list.flatMap( any => List.fill(times)(any))
    assert(duplicateN(3, List('a, 'b, 'c, 'c, 'd)) === List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  test("Problem 16: Drop every Nth element from a list.") {
    def dropAcum(nth: Int, i: Int, list: List[Any]): List[Any] = list match {
      case Nil => Nil
      case x :: xs => if (i == 1) dropAcum(nth, nth, xs) else x :: dropAcum(nth, i-1, xs)
    }
    def drop(nth: Int, list: List[Any]) = dropAcum(nth, nth, list)
    assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("Problem 17: Split a list into two parts.") {
    def split(where: Int, list: List[Any]) = (list.take(where), list.drop(where))
    assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  test("Problem 18: Extract a slice from a list.") {
    def slice(from: Int, to: Int, list: List[Any]) = list.slice(from, to)
    assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('d, 'e, 'f, 'g))
  }

  test("Problem 19: Rotate a list N places to the left.") {
    def rotate(places: Int, list: List[Any]): List[Any] = places compare 0 match {
      case 0  => list
      case 1  => rotate(places - 1, list.tail ++ List(list.head))
      case -1 => rotate(places + 1, list.last :: list.init)
    }
    assert(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    assert(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  test("Problem 20: Remove the Kth element from a list.") {
    def removeAt(nth: Int, list: List[Any]) = (list.take(nth) ++ list.drop(nth + 1), list(nth))
    assert(removeAt(1, List('a, 'b, 'c, 'd)) === (List('a, 'c, 'd),'b))
  }

  test("Problem 21: Insert an element at a given position into a list.") {
    def insertAt[A](elem: A, pos: Int, list: List[A]) = list.take(pos) ++ (elem :: list.drop(pos))
    assert(insertAt('new, 1, List('a, 'b, 'c, 'd)) === List('a, 'new, 'b, 'c, 'd))
  }

  test("Problem 22: Create a list containing all integers within a given range.") {
    def range(from: Int, to: Int) = List.range(from, to + 1)
    assert(range(4, 9) === List(4, 5, 6, 7, 8, 9))
  }

  test("Problem 23: Extract a given number of randomly selected elements from a list.") {
    //Hint: Use the solution to problem P20
    def removeAt[A](nth: Int, list: List[A]) = (list.take(nth) ++ list.drop(nth + 1), list(nth))
    def randomSelect[A](qty: Int, list: List[A]): List[A] = list match {
      case Nil => Nil
      case l => if (qty == 0) Nil else {
        val tuple = removeAt(Random.nextInt(l.size), l)
        tuple._2 :: randomSelect(qty - 1, tuple._1)
      }
    }

    val source = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    val result = randomSelect(3, source)
    assert(result.size === 3)
    assert(result.forall( p => source.contains(p)))
  }

  test("Problem 24: Lotto: Draw N different random numbers from the set 1..M.") {
    def lotto(qty: Int, max: Int) = List.fill(qty)(Random.nextInt(max)+1)

    val qty = 6
    val max = 49
    val result = lotto(qty, max)
    assert(result.size === qty)
    assert(result.forall( i => i >= 1 && i <= max))
  }

  test("Problem 25: Generate a random permutation of the elements of a list.") {
    //Hint: Use the solution of problem P23
    def removeAt[A](nth: Int, list: List[A]) = (list.take(nth) ++ list.drop(nth + 1), list(nth))
    def randomSelect[A](qty: Int, list: List[A]): List[A] = list match {
      case Nil => Nil
      case l => if (qty == 0) Nil else {
        val tuple = removeAt(Random.nextInt(l.size), l)
        tuple._2 :: randomSelect(qty - 1, tuple._1)
      }
    }
    def randomPermute(list: List[Any]) = randomSelect(list.size, list)

    val source = List('a, 'b, 'c, 'd, 'e, 'f)
    val result = randomPermute(source)
    assert(result.size === source.size)
    assert(result.forall( i => source.contains(i)))
  }

  test("Problem 26: Generate the combinations of K distinct objects chosen from the N elements of a list.") {
    def factorial(num: Int) = {
      @tailrec def facAcum(num: Int, acum: Int): Int = if (num == 0) acum else facAcum(num - 1, acum * num)
      facAcum(num, 1)
    }
    def combinatorial(qty: Int, group: Int) = factorial(qty) / (factorial(group) * factorial(qty - group))
    def combinations[A](qty: Int, list: List[A]): List[List[A]] = {
        if (qty == 0)
          Nil
        else if (qty == 1)
          for (elem <- list) yield List(elem)
        else
        for (elem <- list;
             recur <- combinations(qty - 1, list.drop(list.indexOf(elem) + 1))
        ) yield (elem :: recur)
    }

    val source = List('a, 'b, 'c, 'd, 'e, 'f)
    val group = 3
    val result = combinations(group, List('a, 'b, 'c, 'd, 'e, 'f))
    assert(result.size === combinatorial(source.size, group))
    assert(result.forall( comb => comb.size == group && comb.forall( i => source.contains(i))))
  }

  test("Problem 27: Group the elements of a set into disjoint subsets.") {
    def group3[A](list: List[A]): List[List[List[A]]] = ???

    val source = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val result = group3(source)
    result.foreach( group => {
      assert(group.size === 3)

    })
  }

  test("Problem 28: Sorting a list of lists according to length of sublists.") {
    def lsort(list: List[List[Any]]) = list.sortBy( list => list.size)
    def frequencyMap[A](list: List[A]): Map[A, Int] = list.foldLeft(Map[A,Int]() withDefaultValue 0) {
      (map, a) => map + ( a -> ( 1 + map(a) ) )
    }
    def lsortFreq(list: List[List[Any]]) = {
      val frequency = frequencyMap(list.map(l => l.size))
      object ListOrdering extends Ordering[List[Any]] {
        override def compare(x: List[Any], y: List[Any]): Int = Ordering.Int.compare(frequency(x.size), frequency(y.size))
      }
      list.sorted(ListOrdering)
    }

    val source = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    assert(lsort(source) === List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))

    assert(lsortFreq(source) === List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)))
  }
}

