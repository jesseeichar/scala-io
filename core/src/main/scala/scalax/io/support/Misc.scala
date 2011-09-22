package scalax.io.support

object Misc {
  def safeSum(numbers : Long*) = (0L /: numbers) { (next,acc) =>
      val sum = acc + next
      if(sum < acc) Long.MaxValue
      else sum
    }
}