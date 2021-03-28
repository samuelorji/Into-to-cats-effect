package ch_06

import cats.{Applicative, Functor}
import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp}
import cats.instances._
import cats.implicits._

import java.util.concurrent.{Callable, Executors, TimeUnit}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

object Async extends App {


 val res =  EitherT.fromOption[List].apply(Some(43),"err")

  val ec = ExecutionContext.Implicits.global

  val futApp = new Applicative[Future] {
    override def pure[A](x: A): Future[A] = {
      println("calling pure")
      Future.successful(x)
    }

    override def ap[A, B](ff: Future[A => B])(fa: Future[A]): Future[B] = {
      println("calling ap")
      fa.flatMap(a => ff.map(f => f(a))(ec))(ec)
    }
  }

  val futFunc = new Functor[Future]{
    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = {
      println("calling map")
      fa.map(f)(ec)
    }
  }

  def getFromGoogle : Future[Option[String]] = Future {
    Thread.sleep(200)
    Some("From google")
  }(ec)


// val rest =   EitherT.fromOption[Future].apply(Some("endpoint"),{println("error case"); 2})(futApp)

  val rest2 = EitherT.fromOptionF(getFromGoogle, -1)(futFunc)

 rest2.value.onComplete(println)(ec)


  Thread.sleep(1000)



}

object Asnc2 extends IOApp {
  val ec = Executors.newCachedThreadPool()

  def getMagicNumber() : Int = {
    Thread.sleep(500)
    43
  }

 def asynComputation =  ec.submit{
    new Callable[Int] {
      override def call(): Int = getMagicNumber()
    }
  }

 val magicIO =  IO.async[Int]{ cb =>
    try cb {
      val result = Right(
        asynComputation.get(1, TimeUnit.SECONDS)
      )
      ec.shutdown()
      result
    }
    catch {
      case NonFatal(e) => cb(Left(e))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _           <- IO(println("Let's calculate the magic number"))
      magicNumber <- magicIO
      _           <- IO(println(s"magic number is $magicNumber"))
    } yield ExitCode.Success
  }
}
