package ch_07

import cats.effect._
import cats.effect.implicits._
import cats.syntax._
import cats.implicits._
import scala.concurrent.duration._

import cats._
object ResourcesExample extends IOApp {

  trait FileApi {
    def getContents : Array[Byte]
    def close : Unit
  }

  def getFileApi = new FileApi {
    override def getContents: Array[Byte] = {
      "hello there".getBytes
    }

    override def close: Unit =  {
      println("closing this file")
    }
  }

  val test = (IO(23), IO(23)).tupled
  val intResource: Resource[IO, Int] = Resource.make(IO(42))(x => IO(println(s"releasing $x ")) *> IO.unit)
  val stringResource: Resource[IO, String] = Resource.make(IO("thor"))(x => IO(println(s"releasing $x ")) *>  IO.unit)

  val result : IO[Unit]= for {
    result <- intResource.use { age =>
      stringResource.use {name =>
        IO(s"name is $name, and age is $age")
      }
    }
    _ <- IO(println(s"result is $result"))
  } yield ()
  val fileResource: Resource[IO, FileApi] = Resource.make(IO(getFileApi))(api => IO.pure(api.close))
  val fileResource2: Resource[IO, FileApi] = Resource.make(IO.raiseError[FileApi](new Exception("Nope")))(api => IO.pure(api.close))

  override def run(args: List[String]): IO[ExitCode] = result.as(ExitCode.Success)
//    for {
//    _               <- IO(println("Let's check out file for your welcome message"))
//    welcomeMessage  <- fileResource.use { fileApi =>
//                        IO(fileApi.getContents)
//                      }
//    _               <- IO(println(s"Your welcome message is [${new String(welcomeMessage)}]"))
//  } yield ExitCode.Success

//  for {
//    _               <- IO(println("Let's check out file for your welcome message"))
//    welcomeMessage  <- fileResource2.use { fileApi =>
//      //IO.raiseError[String](new Exception("are we gonna be released ????"))
//      IO(fileApi.getContents)
//    }
//    _               <- IO(println(s"Your welcome message is [${new String(welcomeMessage)}]"))
//  } yield ExitCode.Success
}

object ResourceEx2 extends IOApp {

  val backgroundTask : Resource[IO, Fiber[IO, Unit]] = {
    def loopTask : IO[Unit] = IO(println("looping .... ")) *> IO.sleep(200 millis) *> IO.suspend(loopTask)
    Resource.make(IO(println("acquiring fiber")) *> loopTask.start)(fiber => IO(println("cancelling background task")) >> fiber.cancel)
  }
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- backgroundTask.use{ _ =>
        IO.sleep(1 second) >> IO(println("Hello from here "))

      }
    } yield ExitCode.Success
}
