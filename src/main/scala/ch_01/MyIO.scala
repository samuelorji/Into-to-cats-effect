package ch_01

import java.util.concurrent.TimeUnit

import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.Future
import scala.concurrent.duration._
import cats.implicits._
import cats.syntax._


object TickingClock extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    tickingClock.as(ExitCode.Success)

  def showTime = println(System.currentTimeMillis())

  val tickingClock: IO[Unit] = IO(showTime) *> IO.sleep(1 second) *> IO.suspend(tickingClock)
  //val tickingClock2: IO[Unit] = IO(showTime) >> IO.sleep(1 second) >> tickingClock

 // val tickingClock: IO[Unit] = IO(showTime).flatMap(_ => IO.sleep(1 second)).flatMap(_ => tickingClock)
//    for {
//      _ <- IO(showTime)
//      _ <- IO.sleep(1 second)
//      _ <- tickingClock
//    } yield ()

}
object Playground  extends App {


  val io = IO.delay("3")
  IO.apply("a")


  val err  : IO[Int] = IO.raiseError[Int](new Exception(""))


  val opt1 = Option(1)
  val opt2 = Option(2)
  val opt3 = Option(3)

  val result2 = (opt1,opt2,opt3).mapN((a,b,c) => a + b + c)

  val result = for {
    a <- opt1
    b <- opt2
    c <- opt3
  } yield a+b+c

  val res = (Option(1), None : Option[Int], Option(3)).mapN(_ + _ + _ + 1)

  println(res)

  val list = List("hello")

  list.as("suck")
  list.void
}
case class MyIO[A](unsafeRun : () => A) {
  def map[B](f : A => B) : MyIO[B] = {
    MyIO(() => f(unsafeRun()))
  }

  def flatMap[B](f : A => MyIO[B]) : MyIO[B] = {
    MyIO(() => f(unsafeRun()).unsafeRun())
  }
}
object MyIO {

  def putStr(line : => String) : MyIO[Unit] = {
    MyIO(() => println(line))
  }

}


object PlayHere extends App {

  val hello = MyIO.putStr("Hello")
  val world = MyIO.putStr("world")


  val result = for {
    _ <- hello
    _ <- world
  } yield ()


  result.unsafeRun()


}

object Timing extends App {
  val clock: MyIO[Long] = {
    MyIO(() => System.currentTimeMillis())
  }

  // Write a timer that records the duration of another action
  def time[A](action: MyIO[A]): MyIO[(FiniteDuration, A)] = {
    for {
      start <- clock
       res <- action
      end <- clock
    } yield (FiniteDuration((end- start),TimeUnit.MILLISECONDS), res)
  }
  val timedHello = Timing.time(MyIO.putStr("hello"))
  timedHello.unsafeRun() match {
    case (duration, _) => println(s"'hello' took $duration")
  }
}

