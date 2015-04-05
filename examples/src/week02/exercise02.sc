def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

def sumSquare(a: Int, b: Int) = sum(x => x * x, 1, 3)

sumSquare(1, 3)

