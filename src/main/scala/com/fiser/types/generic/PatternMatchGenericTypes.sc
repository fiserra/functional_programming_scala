def extractString(a: Any): Option[String] = a match {
  case s: String => Some(s)
  case _ => None
}

sealed trait Result[+T]

case class Ok[+T](values: List[T]) extends Result[T]

case class Fail(message: String) extends Result[Nothing]

def handleResult(result: Result[String]): Unit = result match {
  case Fail(message) => println("Error: " + message)
  case Ok(vs: List[String]) => println("Got strings of total length: " + vs.map(_.length).sum)
  case _ => println("Got something else")
}

handleResult(Fail("something happened"))
handleResult(Ok(List("Cucu", "rigu")))


case class Strings(values: List[String])
case class Ints(values: List[Int])
def handle(a: Any): Unit = a match {
  case Strings(vs) => println("strings: " + vs.map(_.size).sum)
  case Ints(vs)    => println("ints: " + vs.sum)
  case _ =>
}

handle(Strings(List("hello", "world")))
handle(Ints(List(1, 2, 3)))
handle(Strings(List("foo", "bar", 4)))