type Set = Int => Boolean

def contains(s: Set, elem: Int): Boolean = s(elem)

val neg = (x: Int) => x < 0

def singletonSet(elem: Int): Set = (x: Int) => x == elem

contains(singletonSet(1), 1)
contains(singletonSet(1), 2)
contains(singletonSet(1), 3)
contains(singletonSet(1), 4)

def union(s: Set, t: Set): Set =
  (x: Int) => contains(s, x) || contains(t, x)

val s = singletonSet(1)
val t = singletonSet(3)
val k = singletonSet(5)

val mix = union(s, t)
val mix2 = union(s, k)

contains(mix, 1)
contains(mix, 2)
contains(mix, 3)
contains(mix, 4)

contains(mix2, 1)
contains(mix2, 2)
contains(mix2, 3)
contains(mix2, 4)
contains(mix2, 5)

def intersect(s: Set, t: Set): Set = (x: Int) => contains(s, x) && contains(t, x)

val inter = intersect(mix, mix2)

contains(inter, 1)
contains(inter, 2)
contains(inter, 3)
contains(inter, 4)
contains(inter, 5)

def diff(s: Set, t: Set): Set = (x: Int) => contains(s, x) && !contains(t, x)

val d1 = diff(mix, mix2)
val d2 = diff(mix2, mix)

contains(d1, 1)
contains(d1, 2)
contains(d1, 3)
contains(d1, 4)
contains(d1, 5)

contains(d2, 1)
contains(d2, 2)
contains(d2, 3)
contains(d2, 4)
contains(d2, 5)

def filter(s: Set, p: Int => Boolean): Set =
  (x: Int) => contains(s, x) && p(x)

val bar = union(union(union(singletonSet(1), singletonSet(2)), singletonSet(3)), singletonSet(4))

contains(bar, 1)
contains(bar, 2)
contains(bar, 3)
contains(bar, 4)
contains(bar, 5)
contains(bar, 6)

def f1(x: Int) = x % 2 == 1
def f2(x: Int) = x > 2

contains(filter(bar, f1), 1)
contains(filter(bar, f1), 2)
contains(filter(bar, f1), 3)
contains(filter(bar, f1), 4)
contains(filter(bar, f1), 5)

contains(filter(bar, f2), 1)
contains(filter(bar, f2), 2)
contains(filter(bar, f2), 3)
contains(filter(bar, f2), 4)
contains(filter(bar, f2), 5)

val bound = 1000

def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a == bound + 1) true
    else if (contains(s, a) && !p(a)) false
    else iter(a + 1)
  }
  iter(-bound)
}

val bar1 =
  union(union(union(singletonSet(1), singletonSet(2)), singletonSet(3)), singletonSet(4))
val bar2 =
  union(union(singletonSet(1), singletonSet(3)), singletonSet(5))

forall(bar1, f2)
forall(bar2, f2)

def exists(s: Set, p: Int => Boolean): Boolean = {
  def p1 = (x: Int) => !p(x)
  !forall(s, p1)
}

def g1(x: Int) = x > 5
def g2(x: Int) = x == 5

exists(bar1, g1)
exists(bar1, g2)
exists(bar2, g1)
exists(bar2, g2)

def map(s: Set, f: Int => Int): Set =
  (x: Int) => exists(s, (y: Int) => f(y) == x)

def h1(x: Int) = x + 1
def h2(x: Int) = x * 2

val mtest1 = map(bar1, h1)
val mtest2 = map(bar1, h2)

contains(mtest1, 1)
contains(mtest1, 2)
contains(mtest1, 3)
contains(mtest1, 4)
contains(mtest1, 5)
contains(mtest1, 6)
contains(mtest1, 7)
contains(mtest1, 8)
contains(mtest1, 9)
contains(mtest1, 10)

contains(mtest2, 1)
contains(mtest2, 2)
contains(mtest2, 3)
contains(mtest2, 4)
contains(mtest2, 5)
contains(mtest2, 6)
contains(mtest2, 7)
contains(mtest2, 8)
contains(mtest2, 9)
contains(mtest2, 10)