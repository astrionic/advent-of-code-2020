package astrionic.adventofcode2020.solutions.day23

private[day23] class CupCollection(
    private val array: Array[Int],
    private val max: Int,
    private var current: Int = 1
) {

  def move(): Unit = {
    val next = array(current)
    val afterNext = array(next)
    val afterAfterNext = array(afterNext)
    val afterRemove = array(afterAfterNext)

    // Skip the removed cups
    array(current) = afterRemove

    // Find destination cup
    var dest = current - 1
    while(Set(next, afterNext, afterAfterNext).contains(dest) || dest < 1) {
      if(dest <= 1) dest = max
      else dest -= 1
    }

    // Reinsert removed cups
    val afterDest = array(dest)
    array(dest) = next
    array(afterAfterNext) = afterDest

    // Update current
    current = array(current)
  }

  def move(n: Int): Unit = for(_ <- 0 until n) move()

  def resultPart1: String = {
    var curr = 1
    val result = new StringBuilder
    for(_ <- 0 until 8) {
      curr = array(curr)
      result.append(curr)
    }
    result.toString()
  }

  def resultPart2: String = {
    val afterOne = array(1)
    val afterAfterOne = array(afterOne).toLong
    (afterOne * afterAfterOne).toString
  }
}

private[day23] object CupCollection {
  def fromList(list: List[Int], maximum: Int = 0): CupCollection = {
    val max = Math.max(list.length, maximum)
    val a = new Array[Int](max + 1)

    def get(i: Int): Int = {
      if(i == max + 1) {
        list.head
      } else if(i <= list.length) {
        list(i - 1)
      } else {
        i
      }
    }

    for(i <- 2 to max + 1) {
      val curr = get(i - 1)
      val next = get(i)
      a(curr) = next
    }

    new CupCollection(a, max)
  }
}
