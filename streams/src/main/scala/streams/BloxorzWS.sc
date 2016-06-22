val test = Stream(1,2,3,4)
val test2 = Stream(10,11,12)

test.head
test.tail.length

(test ++ test2).length
test.head #:: test2

def from(x: Int): Stream[Int] = x #:: from(x + 1)

def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

val prime = sieve(from(2)).tail.tail.tail.tail.head