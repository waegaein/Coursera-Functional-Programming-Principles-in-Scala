package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
    * how often the character appears.
    * This list is sorted alphabetically w.r.t. to the character in each pair.
    * All characters in the occurrence list are lowercase.
    *
    * Any list of pairs of lowercase characters and their frequency which is not sorted
    * is **not** an occurrence list.
    *
    * Note: If the frequency of some character is zero, then that character should not be
    * in the list.
    */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
    * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
    *
    * Note: the uppercase and lowercase version of the character are treated as the
    * same character, and are represented as a lowercase character in the occurrence list.
    */
  def wordOccurrences(w: Word): Occurrences = {
    def extractor(item: (Char, List[Char])): (Char, Int) = (item._1, item._2.length)
    w.toLowerCase.toList.groupBy((element: Char) => element).toList.map(extractor).sorted
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    def glue(s: Sentence): Word = s match {
      case Nil => ""
      case hd :: tl => hd.concat(glue(tl))
    }
    wordOccurrences(glue(s))
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
    * the words that have that occurrence count.
    * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
    *
    * For example, the word "eat" has the following character occurrence list:
    *
    * `List(('a', 1), ('e', 1), ('t', 1))`
    *
    * Incidentally, so do the words "ate" and "tea".
    *
    * This means that the `dictionaryByOccurrences` map will contain an entry:
    *
    * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    *
    */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy((element: Word) => wordOccurrences(element))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
    * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
    * is a subset of `List(('k', 1), ('o', 1))`.
    * It also include the empty subset `List()`.
    *
    * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
    *
    * List(
    * List(),
    * List(('a', 1)),
    * List(('a', 2)),
    * List(('b', 1)),
    * List(('a', 1), ('b', 1)),
    * List(('a', 2), ('b', 1)),
    * List(('b', 2)),
    * List(('a', 1), ('b', 2)),
    * List(('a', 2), ('b', 2))
    * )
    *
    * Note that the order of the occurrence list subsets does not matter -- the subsets
    * in the example above could have been displayed in some other order.
    */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def combinations_sub(sofar: List[Occurrences], aim: Char, cur: Int): List[Occurrences] =
      if (cur == 0) {
        if (sofar.isEmpty) List(List()) else sofar
      }
      else {
        if (sofar.isEmpty) List((aim, cur)) :: combinations_sub(sofar, aim, cur - 1)
        else sofar.map((x: Occurrences) => (aim, cur) :: x).union(combinations_sub(sofar, aim, cur - 1))
      }
    def proceed(occurrences: Occurrences): List[Occurrences] = occurrences match {
      case Nil => List(List())
      case hd :: tl => combinations_sub(proceed(tl), hd._1, hd._2)
    }
    proceed(occurrences)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
    *
    * The precondition is that the occurrence list `y` is a subset of
    * the occurrence list `x` -- any character appearing in `y` must
    * appear in `x`, and its frequency in `y` must be smaller or equal
    * than its frequency in `x`.
    *
    * Note: the resulting value is an occurrence - meaning it is sorted
    * and has no zero-entries.
    */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def subtract_sub(x: Occurrences, cur: (Char, Int)): Occurrences = x match {
      case Nil => Nil
      case hd :: tl =>
        if (hd._1 == cur._1) {
          if (hd._2 == cur._2) tl
          else (hd._1, hd._2 - cur._2) :: tl
        }
        else hd :: subtract_sub(tl, cur)
    }
    def proceed(x: Occurrences, y: Occurrences): Occurrences = y match {
      case Nil => x
      case hd :: tl => proceed(subtract_sub(x, hd), tl)
    }
    proceed(x, y).sorted
  }

  /** Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the occurrences of all the characters of
    * all the words in the sentence, and producing all possible combinations of words with those characters,
    * such that the words have to be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have to correspond.
    * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order are considered two different anagrams.
    * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
    * `List("I", "love", "you")`.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
    *
    * List(
    * List(en, as, my),
    * List(en, my, as),
    * List(man, yes),
    * List(men, say),
    * List(as, en, my),
    * List(as, my, en),
    * List(sane, my),
    * List(Sean, my),
    * List(my, en, as),
    * List(my, as, en),
    * List(my, sane),
    * List(my, Sean),
    * List(say, men),
    * List(yes, man)
    * )
    *
    * The different sentences do not have to be output in the order shown above - any order is fine as long as
    * all the anagrams are there. Every returned word has to exist in the dictionary.
    *
    * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
    * so it has to be returned in this list.
    *
    * Note: There is only one anagram of an empty sentence.
    */
  def dictCheck(item: Occurrences) = dictionaryByOccurrences get item

  def defectCheck(total: Occurrences, target: List[Occurrences]): Boolean =
    if (target.isEmpty) {
      !(total.isEmpty)
    }
    else defectCheck(subtract(total, target.head), target.tail)

  def defectKill(total: Occurrences, target: List[List[Occurrences]]): List[List[Occurrences]] = target match {
    case Nil => Nil
    case hd :: tl => if (defectCheck(total, hd)) defectKill(total, tl) else hd :: defectKill(total, tl)
  }

  def makePath_sub(occur: Occurrences, subsets: List[Occurrences]): List[List[Occurrences]] = subsets match {
    case Nil => Nil
    case hd :: tl => dictCheck(hd) match {
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
          else next.map((x: List[Occurrences]) => hd :: x).union(makePath_sub(occur, tl))
        }
      }
    }
  }

  def makePath(sentence: Sentence) = {
    val occur = sentenceOccurrences(sentence)
    val subsets = combinations(occur)

    defectKill(occur, makePath_sub(occur, subsets))
  }

  def increment(cur: List[Word], sofar: List[Sentence]): List[Sentence] = cur match {
    case Nil => Nil
    case hd :: tl => {
      if (sofar.isEmpty) List(List(hd)).union(increment(tl, sofar))
      else sofar.map((x: Sentence) => hd :: x).union(increment(tl, sofar))
    }
  }

  def convertPath_sub(path: List[Occurrences]): List[Sentence] = path match {
    case Nil => Nil
    case hd :: tl => dictCheck(hd) match {
      case None => Nil
      case Some(e) => increment(e, convertPath_sub(tl))
    }
  }

  def convertPath(paths: List[List[Occurrences]]): List[Sentence] = paths match {
    case Nil => Nil
    case hd :: tl => convertPath_sub(hd).union(convertPath(tl))
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = if (sentence.isEmpty) List(Nil) else convertPath(makePath(sentence))
}