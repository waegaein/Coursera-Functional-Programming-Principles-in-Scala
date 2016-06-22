import forcomp.Anagrams._

wordOccurrences("apple")

val sentence = List("linuxrulez")

val occur = sentenceOccurrences(sentence)
val subsets = combinations(occur)

val test  = List(Nil)

test.isEmpty
test == List(Nil)
//val paths = makePath(total, subsets)
/*

def proceed(occur: Occurrences, subsets: List[Occurrences]): List[List[Occurrences]] = subsets match {
  case Nil => Nil
  case hd::tl => {
    if (subtract(occur, hd).isEmpty) dictCheck(hd) match {
      case None => List(Nil)
      case Some(e) => List(List(hd))
    }
    else dictCheck(hd) match {
      case None => proceed(occur, tl)
      case Some(e) => {
        val nextOccur = subtract(occur, hd)
        val nextSubsets = combinations(nextOccur)
        val next = proceed(nextOccur, nextSubsets)
        if (next == List(Nil)) proceed(occur, tl)
        else next.map( (x: List[Occurrences]) => hd :: x).union(proceed(occur, tl))
      }
    }
  }
}
*/

def makePath(sentence: Sentence) = {
  val occur = sentenceOccurrences(sentence)
  val subsets = combinations(occur)

  defectKill(occur, makePath_sub(occur, subsets))
}

def makePath_sub(occur: Occurrences, subsets: List[Occurrences]): List[List[Occurrences]] = subsets match {
  case Nil => Nil
  case hd::tl => dictCheck(hd) match {
    case None => {
      if (subtract(occur, hd).isEmpty) List(Nil).union(makePath_sub(occur, tl))
      else makePath_sub(occur, tl)
    }
    case Some(e) => {
      if (subtract(occur, hd).isEmpty) List(List(hd)).union(makePath_sub(occur, tl))
      else {
        val nextOccur = subtract(occur, hd)
        val nextSubsets = combinations(nextOccur)
        val next = makePath_sub(nextOccur, nextSubsets)
        if (next == List(Nil)) makePath_sub(occur, tl)
        else next.map( (x: List[Occurrences]) => hd :: x).union(makePath_sub(occur, tl))
      }
    }
  }
}

def defectCheck(total: Occurrences, target: List[Occurrences]): Boolean =
  if (target.isEmpty) {
    if (total.isEmpty) false else true
  }
  else defectCheck(subtract(total, target.head), target.tail)

def defectKill(total: Occurrences, target: List[List[Occurrences]]): List[List[Occurrences]] = target match {
  case Nil => Nil
  case hd::tl => if (defectCheck(total, hd)) defectKill(total, tl) else hd :: defectKill(total, tl)
}

val foo = makePath_sub(occur, subsets)
val foo2 = defectKill(occur, foo)
val foo3 = makePath(sentence)

def increment(cur: List[Word], sofar: List[Sentence]): List[Sentence] = cur match {
  case Nil => Nil
  case hd::tl => {
    if (sofar.isEmpty) List(List(hd)).union(increment(tl, sofar))
    else sofar.map( (x: Sentence) => hd :: x).union(increment(tl, sofar))
  }
}

def convertPath_sub(path: List[Occurrences]): List[Sentence] = path match {
  case Nil => Nil
  case hd::tl => dictCheck(hd) match {
    case None => Nil
    case Some(e) => increment(e, convertPath_sub(tl))
  }
}

def convertPath(paths: List[List[Occurrences]]): List[Sentence] = paths match {
  case Nil => Nil
  case hd::tl => convertPath_sub(hd).union(convertPath(tl))
}

def sentenceAnagrams(sentence: Sentence): List[Sentence] = convertPath(makePath(sentence))



