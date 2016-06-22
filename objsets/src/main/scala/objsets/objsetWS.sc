import objsets._

lazy val trend = GoogleVsApple.trending
lazy val apple = GoogleVsApple.appleTweets.descendingByRetweet
lazy val google = GoogleVsApple.googleTweets.descendingByRetweet

google.head
google.tail.head
google.tail.tail.head
apple.head
apple.tail.head
apple.tail.tail.head
trend.head
trend.tail.head
trend.tail.tail.head
def a = List(1,2,3).max
//a.descendingByRetweet