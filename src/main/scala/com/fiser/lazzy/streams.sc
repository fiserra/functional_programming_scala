import com.fiser.lazzy.Stream

val stream = Stream(2, 3, 4, 56)

stream.toList

stream.take(6).toList
stream.take(2).toList
stream.takeWhile(_ > 92).toList
stream.takeWhile2(_ > 92).toList
stream.takeWhile(_ < 4).toList
stream.takeWhile2(_ < 4).toList

stream.drop(2).toList

stream.exists(_ == 4)
stream.forAll(_ > 10)

stream.map(_.toString).toList
stream.filter(_ <= 3).toList

val ones: Stream[Int] = Stream.cons(1, ones)

ones.take(4).toList
ones.forAll(_ != 1)
ones.forAll(_ == 1)
ones.takeWhile(_ ==  1)