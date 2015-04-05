def factorial(n: Int): Int = {
  def loop(memo: Int, n: Int): Int =
    if (n == 1) memo
    else loop(memo * n, n - 1)
  loop(1, n)
}

factorial(20)