package ch_03

import cats.{Applicative, Parallel, effect}
import cats.effect.{ContextShift, IO}
import cats.implicits._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

object Future2 extends App {

  implicit val ec = ExecutionContext.global

  def sleepFor(time: Long) = Thread.sleep(time)

  def hello = Future {
    sleepFor(1000)
    println(s"[${Thread.currentThread.getName}] Hello")
  }

  def world = Future {
    sleepFor(1000)
    println(s"[${Thread.currentThread.getName}] World")
  }

  val st1 = System.currentTimeMillis()
  val hw1: Future[Unit] = {
    for {
      _ <- hello
      _ <- world
    } yield ()
  }

  Await.ready(hw1, 5.seconds)
  println(s"h1 took ${(System.currentTimeMillis() - st1)} milliseconds")


  val st2 = System.currentTimeMillis()
  val hw2: Future[Unit] =
    (hello, world).mapN((_, _) => ())

  Await.ready(hw2, 5.seconds)
  println(s"h2 took ${(System.currentTimeMillis() - st2)} milliseconds")
}
object Future3 extends App {

  val parApp : Applicative[IO.Par] = new Applicative[effect.IO.Par] {
    override def pure[A](x: A): _root_.cats.effect.IO.Par[A] = IO.Par.apply(IO.pure(x))

    override def ap[A, B](ff: _root_.cats.effect.IO.Par[A => B])(fa: _root_.cats.effect.IO.Par[A]): _root_.cats.effect.IO.Par[B] = ???
  }
//
//  implicit def ap(implicit cs: ContextShift[IO]): Applicative[IO.Par] = {
//    new Applicative[IO.Par] {
//      def pure[A](a: A): IO.Par[A] = IO.Par(a)
//      def map[A, B](pa: IO.Par[A])(f: A => B): IO.Par[B] = ???
//      def product[A, B](pa: IO.Par[A], pb: IO.Par[B]): IO.Par[(A, B)] = ???
//    }
//  }

  implicit val ec = ExecutionContext.global

  def sleepFor(time: Long) = Thread.sleep(time)

  def hello = IO {
    sleepFor(1000)
    println(s"[${Thread.currentThread.getName}] Hello")
  }

  def world = IO {
    sleepFor(1000)
    println(s"[${Thread.currentThread.getName}] World")
  }

  val st1 = System.currentTimeMillis()
  val hw1: IO[Unit] = {
    for {
      _ <- hello
      _ <- world
    } yield ()
  }


  Await.ready(hw1.unsafeToFuture(), 5.seconds)
  println(s"h1 took ${(System.currentTimeMillis() - st1)} milliseconds")


  val st2 = System.currentTimeMillis()
  val hw2: IO[Unit] =
    (hello, world).mapN((_, _) => ())

  Await.ready(hw2.unsafeToFuture(), 5.seconds)
  println(s"h2 took ${(System.currentTimeMillis() - st2)} milliseconds")
}
