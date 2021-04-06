package ch_05

import utils.debug._
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.effect.implicits._
import cats.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object Parallelism extends IOApp {

  def task(i : Int) = IO(i).debug
  val numCPUs = Runtime.getRuntime.availableProcessors()

  val tasks: IO[List[Int]] = List.range(0, 2 * numCPUs).parTraverse(task)
  val program = for {
    _ <- IO(s"Number of CPUs : $numCPUs").debug
    _ <- tasks.debug
  } yield ()


 val prog2 =
      for {
        _ <- IO("one").debug
        _ <- IO.shift
        _ <- IO("two").debug
        _ <- IO.shift
        _ <- IO("three").debug
        _ <- IO.shift
        _ <- IO("4").debug
        _ <- IO.shift
        _ <- IO("5").debug
        _ <- IO.shift
        _ <- IO("6").debug
        _ <- IO.shift
        _ <- IO("7").debug
        _ <- IO.shift
        _ <- IO("8").debug
        _ <- IO.shift
        _ <- IO("9").debug
        _ <- IO.shift
        _ <- IO("10").debug
        _ <- IO.shift
        _ <- IO("11").debug
        _ <- IO.shift
        _ <- IO("12").debug
        _ <- IO.shift
        _ <- IO("13").debug

      } yield ExitCode.Success

 val prog =  Blocker.apply[IO].use{ blocker =>
    withBlocker(blocker).as(ExitCode.Success)
  }

  def withBlocker(blocker : Blocker) : IO[Unit] = {
    for {
      _ <- IO("on IOapp threadpool").debug
      _ <- blocker.blockOn(IO("on blocker").debug)
      _ <- IO("<---- Thread I'm on").debug
    } yield ()
  }


  val ec = ExecutionContext.Implicits.global
  val program = for {
    _ <- IO.shift(ec)
    _ <- IO(println(s"I am running on ${Thread.currentThread().getName}"))
  } yield ()
  override def run(args: List[String]): IO[ExitCode] =
    //program.as(ExitCode.Success)
  program3.as(ExitCode.Success)

}
