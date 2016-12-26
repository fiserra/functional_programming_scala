package com.fiser

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

package object utils {

  implicit class FutureAwaitOps[A](f: Future[A]) {
    def await: A = Await.result(f, Duration.Inf)
  }

}
