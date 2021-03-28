package ch_08

import cats.effect.{IO, _}
import cats.effect.implicits._

import scala.concurrent.duration._
import cats.syntax._
import cats.implicits._
import cats._
import cats.effect.concurrent.{Deferred, Ref}
import utils.debug.DebugHelper

object Refs extends IOApp {

//  var counter = 0
//  def tickingClocks(name : String) : IO[Unit] = {
//    for {
//      _ <- IO(println(s"[$name] : ${System.currentTimeMillis()}"))
//      _ =  counter = counter + 1
//      _ <- IO.sleep(1 second)
//      _ <- tickingClocks(name)
//    } yield ()
//  }
//
//
//  def printCounter() : IO[Unit] =
//    for {
//      _ <- IO(println(s"counter is $counter"))
//      _ <- IO.sleep(1 second)
//      _ <- printCounter()
//    } yield ()
//
//
//  val program : IO[ExitCode] = (tickingClocks("first clock"), tickingClocks("second clock"), printCounter()).parTupled.as(ExitCode.Success)


//    def tickingClocks(name : String, counter : Ref[IO,Long], alerter : Deferred[IO,Unit]) : IO[Unit] = {
//      for {
//        _ <- IO(println(s"[$name] : ${System.currentTimeMillis()}"))
//        counterValue <- counter.updateAndGet(_ + 1)
//        _ <- if (counterValue >= 13) alerter.complete(()).attempt.void else IO.unit
//        _ <- IO.sleep(1 second)
//        _ <- tickingClocks(name,counter,alerter)
//      } yield ()
//    }

  // Attempts to modify the current value once,
  // returning false if another concurrent modification
  // completes between the time the variable is read and the time it is set.

  def tickingClocks(name : String, counter : Ref[IO,Long], alerter : Deferred[IO,Unit]) : IO[Unit] = {
    for {
      _ <- IO(println(s"[$name] : ${System.currentTimeMillis()}"))
      counterValue <- counter.updateAndGet(_ + 1)
      _ <- if (counterValue >= 13) alerter.complete(()) else IO.unit
      _ <- IO.sleep(1 second)
      _ <- tickingClocks(name,counter,alerter)
    } yield ()
  }

  def alertIf13(is13 : Deferred[IO,Unit]) : IO[Unit] = {
    for {
      _ <-  is13.get
      _ <- IO(println("ALERT!!!!!!!"))
    } yield ()
  }

    def printCounter(counter : Ref[IO,Long]) : IO[Unit] =
      for {
        counterValue <- counter.get
        _            <- IO(println(s"counter is $counterValue"))
        _            <- IO.sleep(1 second)
        _            <- printCounter(counter)
      } yield ()


  override def run(args: List[String]): IO[ExitCode] =
    for {
      ref     <- Ref[IO].of(0L)
      alerter <- Deferred[IO,Unit]
          _   <- (
            tickingClocks("first clock",ref, alerter),
            tickingClocks("second clock",ref, alerter),
            alertIf13(alerter),
            printCounter(ref)
            ).parTupled
    } yield ExitCode.Success
}

object Latching extends IOApp {

  trait CountdownLatch {
    def await(): IO[Unit]
    def decrement(): IO[Unit]
  }

  sealed trait LatchState
  case class CountingDown(count : Long) extends LatchState
  case object Done extends LatchState


//  sealed trait State
//  case class Outstanding(n: Long, whenDone: Deferred[IO, Unit]) extends State
//  case class Done() extends State


  def actionWithPrerequisites(latch: CountdownLatch) =
    for {
      _ <- IO(println("waiting for prerequisites"))
      _ <- latch.await
      result <- IO("action")
    } yield result


  def runPrerequisite(latch: CountdownLatch) =
    for {
      result <- IO(println("prerequisite"))
      _ <- latch.decrement
    } yield result

  object CountdownLatch {
    def apply(n: Int): IO[CountdownLatch] = {
      require(n > 0, "number of latches should be greater than 0")
      for {
        latchSignal <- Deferred[IO, Unit]
        latchState <- Ref[IO].of[LatchState](CountingDown(n))
      } yield new CountdownLatch {
        override def await(): IO[Unit] =
          latchSignal.get

        override def decrement(): IO[Unit] = {
          latchState.modify {
            case CountingDown(1) =>
                // last latch
                Done -> latchSignal.complete(())

            case res@CountingDown(count) =>
              res.copy(count - 1) -> IO.unit

            case Done =>
              Done -> IO.unit

          }.flatten
        }
      }
    }
  }


//  object CountdownLatch {
//    def apply(n : Int) : IO[CountdownLatch] = {
//      require(n > 0 , "number of latches should be greater than 0")
//      for {
//        latchSignal <- Deferred[IO,Unit]
//        latchState   <- Ref[IO].of[LatchState](CountingDown(n))
//      } yield new CountdownLatch {
//        override def await(): IO[Unit] =
//          latchSignal.get
//
//        override def decrement(): IO[Unit] = {
//          latchState.updateAndGet {
//            case CountingDown(1) | Done =>
//              // last latch
//              Done
//
//            case res @CountingDown(count) =>
//              res.copy(count - 1)
//          }.flatMap {
//            case Done =>
//              // used attempt because of multiple calls to attempt
//              latchSignal.complete(()).attempt.void
//            case _ => IO.unit
//          }
//        }
//      }
//    }
//  }

//  object CountdownLatch {
//    def apply(n: Long): IO[CountdownLatch] =
//      for {
//        whenDone <- Deferred[IO, Unit]
//        state <- Ref[IO].of[State](Outstanding(n, whenDone))
//      } yield new CountdownLatch {
//        def await(): IO[Unit] =
//          state.get.flatMap {
//            case Outstanding(_, _) => whenDone.get
//            case Done() => IO.unit
//          }
//        def decrement(): IO[Unit] =
//          state.modify {
//            case Outstanding(1, whenDone) => Done() -> whenDone.complete(())
//            case Outstanding(n, whenDone) =>
//              Outstanding(n - 1, whenDone) -> IO.unit
//            case Done() => Done() -> IO.unit
//          }.flatten
//      }
//  }

  def run(args: List[String]): IO[ExitCode] =
    for {
      latch <- CountdownLatch(3)
      _     <- (
            tickingClocks("first clock", latch),
            tickingClocks("second clock", latch),
            alertIf13(latch),
            ).parTupled
    } yield ExitCode.Success


  def tickingClocks(name : String, latch : CountdownLatch) : IO[Unit] = {
    for {
      _ <- IO.sleep(1 second)
      _ <- IO(println(s"[$name] : ${System.currentTimeMillis()}"))
      _ <- latch.decrement()
      _ <- tickingClocks(name,latch)
    } yield ()
  }

  def alertIf13(latch :CountdownLatch) : IO[Unit] = {
    for {
      _ <-  latch.await()
      _ <- IO(println("ALERT!!!!!!!"))
    } yield ()
  }

  def beeper(latch: CountdownLatch) =
    for {
      _ <- latch.await
      _ <- IO("BEEP!").debug
    } yield ()
  def tickingClock(latch: CountdownLatch): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis).debug
      _ <- latch.decrement
      _ <- tickingClock(latch)
    } yield ()
}
