package utils

import java.time.Instant
import java.util.Date
import java.util.concurrent.{Callable, Executor, ExecutorService, Executors, Future}

object Play extends App {

  val runnable = new Runnable {
    def printThread = {
      try {
        //Thread.sleep(1000)
        println(s"[${Instant.now()}] Hello from thread ${Thread.currentThread().getName}")
        Thread.sleep(1000)
        //run()
      } catch {
        case _ : InterruptedException =>
          println(s"[${Instant.now()}] ${Thread.currentThread().getName}: Thread interrupted")
          //run()

      }
    }
    override def run(): Unit = printThread

  }
  val thread = new Thread(runnable)
//
//  thread.start()
//
//
//  //thread.join()
//  thread.interrupt()
//  Thread.sleep(500)
//  thread.join()
  //thread.start()


  val executorService = Executors.newCachedThreadPool()

//  val runnable = new Runnable {
//    override def run(): Unit = {
//      Thread.sleep(1000)
//    }
//  }

  val callable = new Callable[String] {
    override def call(): String =  {
      Thread.sleep(1000)
      "Hello there"
    }
  }
//  val future: Future[String] = executorService.submit(callable)
//  println(future.get())
//
//  executorService.shutdown()

  println(Runtime.getRuntime().availableProcessors())

}
