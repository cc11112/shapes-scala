//package edu.luc.cs.laufer.cs473.shapes

import scala.actors._

object ActExample {
  
  class Worker extends Actor {
    def act() {
      Actor.loop {
        react {
          case s: String => reply(s.length)
          case _ => exit()
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val arguments = args.toList
    val workers = arguments.map(_ => (new Worker).start)
    val futures = for ((w, a) <- workers zip arguments) yield w !! a
    val results = futures.map(f => f() match {
      case i: Int => i
      case _ => throw new Exception("Whoops--didn't expect to get that!")
    })
    println(results)
    workers.foreach(_ ! None)
  }
  
}
