object Fibonacci {
  def notcofib(n: Int): Int = {
    if (n < 0) throw new IllegalArgumentException("Negative numbers not allowed")
    else if (n == 0) 0
    else if (n == 1) 1
    else fib(n-2) + fib(n-1)
  }

  def fib(n: Int): Int = {
    def inner( back2: Tuple2[Int, Int], back1: Tuple2[Int, Int]): Tuple2[Int, Int] = {
      if (back1._2 ==n ) back1
      else inner(back1, (back1._1 + back2._1, back1._2 + 1))
    }

    if (n < 0) throw new IllegalArgumentException("Negative numbers not allowed")
    else if (n == 0) 0
    else if (n == 1) 1
    else inner((0, 0), (1, 1))._1
  }

  def main(args: Array[String]): Unit = {
    val myseq = (0 to 10).map(fib)
    myseq.foreach(println)
  }
}
