package ch_10

import cats.data.Chain
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{ContextShift, ExitCase, Fiber, IO}

import java.util.UUID

object Scheduling {

  sealed trait Job



  object Job {
    case class Id(value : UUID) extends AnyVal
    // represents an effect that should be run
    case class Scheduled(id : Id, task : IO[_]) extends Job {
      def start(implicit cs : ContextShift[IO]) : IO[Running] = {
        for {
          exitCase <- Deferred[IO,ExitCase[Throwable]]
          fiber <- task.
            void.  //void task ... (map to unit)
            attempt.
            // guarantee case is like a finalizer, to be run irrespective of the result of the
            // fiber
            guaranteeCase(exitCase.complete).
            start
        } yield Running(id,fiber,exitCase)
      }
    }

    // holds the id of the task being executed
    // fiber representing the executed effect
    // deferred value to
    case class Running(id : Id, fiber : Fiber[IO, Either[Throwable, Unit]], exitCase : Deferred[IO,ExitCase[Throwable]]) extends Job {
      // semantically block until the exit case value has been produced by the running task
      val await : IO[Completed] =
        exitCase.get.map(Completed(id,_))
    }


    // contains actual result as an ExitCase value
    case class Completed(id : Id, exitCase: ExitCase[Throwable]) extends Job

    def create[A](task : IO[A]) : IO[Scheduled] =
      IO(Id(UUID.randomUUID())).map(Scheduled(_,task))
  }

  trait JobScheduler {
    def schedule(task : IO[_]) : IO[Job.Id]
  }

  //scheduler will manage jobs and their states with the user defined state data type

  object JobScheduler {

    case class State(
                      maxRunning: Int,
                      scheduled: Chain[Job.Scheduled] = Chain.empty,
                      running: Map[Job.Id, Job.Running] = Map.empty,
                      completed: Chain[Job.Completed] = Chain.empty
                    ) {
      def enqueue(job : Job.Scheduled) : State = {
        copy(scheduled = scheduled :+  job)
      }

      def updateRunning(job : Job.Running) : State =
        copy(running = running.updated(job.id, job))

      def dequeue : (State, Option[Job.Scheduled]) = {
        if(running.size >= maxRunning) {
          // don't start more jobs unless there's capacity
          this -> None
        } else {
          scheduled.uncons.map {
                //first scheduled job should be started
            case(head , tail) =>
              copy(scheduled = tail) -> Some(head)

          }.getOrElse(this -> None) // no jobs to start
        }
      }
    }


//    def schedule(stateRef: Ref[IO, State]): JobScheduler =
//      new JobScheduler {
//        override def schedule(task: IO[_]): IO[Job.Id] = {
//          for {
//            job <- Job.create(task)
//            _ <- stateRef.update(_.enqueue(job))
//          }
//        }
//      }

    trait Reactor {
      def whenAwake(
                     onStart: Job.Id => IO[Unit],
                     onComplete: (Job.Id, ExitCase[Throwable]) => IO[Unit]
                   ): IO[Unit]
    }

    object Reactor {


      def apply(stateRef : Ref[IO, JobScheduler.State])(implicit cs : ContextShift[IO]) : Reactor = {





        def startNextJob : IO[Option[Job.Running]] = {
          for {
            job <- stateRef.modify(_.dequeu)
          }
        }
        new Reactor {
          override def whenAwake(onStart: Job.Id => IO[Unit], onComplete: (Job.Id, ExitCase[Throwable]) => IO[Unit]): IO[Unit] = {
            def registerOnComplete(job: Scheduling.Job.Running) =
              job.await.flatMap(jobCompleted).start


            def jobCompleted(job : Job.Completed) : IO[Unit] = {
              stateRef.update(_.onComplete(job))
                .flatTap(_ => onComplete(job.id, job.exitCase).attempt)
            }

            def startJob(scheduled : Job.Scheduled) : IO[Job.Running] = {
              for {
                runningJob <- scheduled.start
                _ <- stateRef.update(_.updateRunning(runningJob))
                _ <- registerOnComplete(runningJob)
                _ <- onStart(runningJob.id).attempt
              } yield runningJob

            }
          }
        }
      }
    }
  }

}
