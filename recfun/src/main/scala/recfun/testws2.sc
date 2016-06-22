val money = 7
val coins = List(1,2,3,4,8)

def availableCoins(money: Int, coins: List[Int]) = coins filter (_ <= money)




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
countChange(300,List(5,10,20,50,100,200,500))
countChange(4,List(1,2))