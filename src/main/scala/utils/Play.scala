package utils

import java.time.Instant
import java.util.Date

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

  thread.start()


  //thread.join()
  thread.interrupt()
  Thread.sleep(500)
  thread.join()
  //thread.start()

}
