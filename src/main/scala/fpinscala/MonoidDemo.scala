package fpinscala

object MonoidDemo {

  def isPrime(n: Long): Boolean =
    (n % 2 != 0) && (3 to (n - 1).toInt by 2).foldLeft(true)({
      case (acc: Boolean, k: Int) => (n % k.toLong != 0) && acc
    })
}
