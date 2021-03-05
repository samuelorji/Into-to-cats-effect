package ch_04

import cats.conversions.all.autoWidenFunctor
import cats.effect._
import cats.effect.implicits._

import scala.concurrent.duration._
import cats.implicits._
import cats.syntax._
object Timeout extends IOApp {
  val effect1 = IO("hello")
  val effect2 = IO("hello")



  import cats.effect.syntax._
  def myParMapN[A,B,C](ia : IO[A], ib: IO[B])(f : (A,B) => C) : IO[C] = {
    IO.racePair(ia,ib).flatMap {
      case Left((a, fiberB)) =>
        (IO.pure(a),fiberB.join).mapN(f)

      case Right((fiberA, b)) =>
        (fiberA.join,IO.pure(b)).mapN(f)
    }
  }
  for {
    e1 <- effect1.start
    e2 <- effect2
  } yield ()

  def showTime = println(System.currentTimeMillis())

  val tickingClock: IO[Unit] = (
    IO(showTime) *> IO.sleep(1 second) *> IO(showTime)
    ).onCancel(IO(println("clock is about to be canceled")))

  val ohNoes =
    IO.sleep(2.seconds) *> IO.raiseError(new RuntimeException("oh noes!"))

  val pro =  (tickingClock, ohNoes).parTupled

  def printMessagefromThread(msg : String) = {
    println(s"[${Thread.currentThread().getName}]: $msg")
  }
  val prog =
    for {
      _ <- IO.unit
      _ <- IO(printMessagefromThread("Before fork"))
      fiba <- (IO.sleep(1 second) >> IO(printMessagefromThread("I am running on another thread"))).start
      _ <- IO(printMessagefromThread("After fork"))
      _ <- fiba.join
      _ <- IO(printMessagefromThread("After Join"))
    } yield ()


  val program : IO[Unit] = {
    for {
      name <- IO.pure(scala.io.StdIn.readLine("What is your name:  "))
      _ <- IO.pure(println(s"Hello $name"))
    } yield ()
  }
  def run(args: List[String]): IO[ExitCode] = program.as(ExitCode.Success)
//    for {
//      done <- IO.race(task, timeout)
//      _ <- done match {
//        case Left(_) => IO(println("task: won"))
//        case Right(_) => IO(println("timeout: won"))
//      }
//    } yield ExitCode.Success
//  val task: IO[Unit] = IO.sleep(100 millis) *> IO(println("task"))
//  val timeout: IO[Unit] = IO.sleep(200 millis) *> IO(println("timeout"))



  //task.tim

  def annotatedSleep(name: String, duration: FiniteDuration): IO[Unit] =
    (
      IO(println(s"$name: starting"))  *>
        IO.sleep(duration) *>
  IO(println(s"$name: done"))
  ).onCancel(IO(println(s"$name: cancelled")).void).void

}
