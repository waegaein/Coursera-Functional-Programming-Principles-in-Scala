val money = 4
val coins = List(6, 1, 2, 3, 4, 5)

def availableCoins(money: Int, coins: List[Int]) = coins filter (_ <= money)

availableCoins(money, coins).max

coins.sorted

coins

coins.filter(_ != coins.max)

coins.filterNot(_ == coins.max)