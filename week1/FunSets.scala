package funsets


/**
  * 2. Purely Functional Sets.
  */
object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = { element: Int => elem == element }


  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = { element: Int => contains(s, element) || contains(t, element) }

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = { element: Int => contains(s, element) && contains(t, element) }

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = { element: Int => contains(s, element) && !contains(t, element) }

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = { element: Int => contains(s, element) && p(element) }


  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def inForall(a: Int): Boolean = {
      if (bound < a) true
      else if (contains(s, a) && !p(a)) false
      else inForall(a + 1)
    }

    inForall(-bound)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def inExists(a: Int): Boolean = {
      if (contains(filter(s, p), a)) true
      else if (bound < a) false
      else inExists(a + 1)
    }

    inExists(-bound)
  }

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int):  Set = {
    def inMap(s:Set, resultSet: Set, a:Int): Set = {
      if (a > bound) resultSet
      else if (contains(s, a)) inMap(s, union(resultSet, x=>f(a) == x), a+1)
      else inMap(s, resultSet, a+1)
    }
    inMap(s, x=>false, -bound)
  }

  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }
}
