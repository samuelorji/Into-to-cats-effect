package ch_03

import cats.{Applicative, Parallel, effect}
import utils.debug._
import cats.syntax._
import cats.implicits._
import cats.effect.{ContextShift, ExitCode, IO, IOApp}

import scala.concurrent.{ExecutionContext, Future}

object Parrallel_IO extends IOApp{
  def run(args: List[String]): IO[ExitCode] =
    seq.as(ExitCode.Success)
  val hello = IO("hello").debug
  val world = IO("world").debug
  val seq =
    (hello, world)
      .mapN((h, w) => s"$h $w")
      .debug
}

object ParallelPlay extends App {

  val global = ExecutionContext.Implicits.global

  // context shift used by parallel IO for scheduling tasks on different threads
  implicit val cs : ContextShift[IO] = IO.contextShift(global)

  def sleepFor(time: Long) = Thread.sleep(time)

  def hello: IO[Unit] = IO {
    sleepFor(1000)
    println(s"[${Thread.currentThread.getName}] Hello")
  }

  def world: IO[Unit] = IO {
    sleepFor(1000)
    println(s"[${Thread.currentThread.getName}] World")
  }

  (hello,world).parMapN((_, _) => {
    println(s"computation took ${(System.currentTimeMillis() - startTime)} milliseconds")
  })

  val parallelHello: IO.Par[Unit] = Parallel[IO].parallel(hello) // converting from regular IO to a parallel IO
  val parallelWorld: IO.Par[Unit] = Parallel[IO].parallel(world) // converting from regular IO to a parallel IO

  val startTime = System.currentTimeMillis()
  val parallelResult: IO.Par[Unit] = (parallelHello,parallelWorld).mapN((_, _) => {
    println(s"computation took ${(System.currentTimeMillis() - startTime)} milliseconds")
  })

  val sequentialIO = Parallel[IO].sequential(parallelResult) // turn parallel IO to sequential IO

  sequentialIO.unsafeRunSync()
}

object Traversal {
  case class Connection()

  val username: Option[String] = Some("username")
  val password: Option[String] = Some("password")
  val url: Option[String] = Some("some.login.url.here")

  import cats.implicits._
  // Stub for demonstration purposes
  def attemptConnect(username: String, password: String, url: String): Option[Connection] = None

  val res: Option[Option[Connection]] = Applicative[Option].map3(username,password,url)(attemptConnect)
  val res2: Option[Option[Connection]] = (username,password,url).traverseN(attemptConnect)


}
