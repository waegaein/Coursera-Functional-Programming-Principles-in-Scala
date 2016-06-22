package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  def balance(chars: List[Char]): Boolean = {
    def balance_acc(chars:List[Char], accum:Int) : Boolean = {
      if(chars.isEmpty){
        accum == 0
      }
      else if(chars.head == '('){
        balance_acc(chars.tail, accum + 1)
      }
      else if(chars.head == ')'){
        if(accum == 0) false
        else balance_acc(chars.tail, accum - 1)
      }
      else{
        balance_acc(chars.tail, accum)
      }
    }
    balance_acc(chars, 0)
  }

/*
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def pure(l: List[Char]): List[Char] =
      if (l.isEmpty) l
      else if (l.head == '(') '(' :: pure(l.tail)
      else if (l.head == ')') ')' :: pure(l.tail)
      else pure(l.tail)
    def step(p: List[Char]): List[Char] =
      if (p.isEmpty) p
      else if (p.tail.isEmpty) p
      else if (p.head == '(' && p.tail.head == ')') step(p.tail.tail)
      else if (p.head == '(' && p.tail.head == '(') step(p.head :: step(p.tail))
      else p
    def check(p:List[Char]) = if (p.isEmpty) true else false

    check(step(pure(chars)))
  }
  */

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val acoins = coins.filter(_ <= money)
    if (acoins.isEmpty) {
      if (money == 0) 1
      else 0
    }
    else if (acoins.length == 1) {
      if (money % acoins.head == 0) 1
      else 0
    }
    else countChange(money, acoins.filterNot(_ == acoins.max)) + countChange(money - acoins.max, acoins)
  }
}
