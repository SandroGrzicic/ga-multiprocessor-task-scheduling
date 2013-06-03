package hr.fer.sandrogrzicic.konpro.aktivnost

import java.util.concurrent.atomic.AtomicInteger

/**
 * A task.
 *
 * @param previous the previous linked task
 * @param delay communication delay between finishing the previous task and starting this one,
 *              if the tasks are on different processors
 * @param time time needed to finish this task
 */
case class Task(id: Int, previous: Option[Task], delay: Int, time: Int, height: Int) {

  override lazy val toString = {
    val sb = StringBuilder.newBuilder
    sb.append("T").append(id).append("(")
        .append("h=").append(height)
        .append(", t=" + time)
        .append(if (delay == 0) "" else ", d=" + delay)
        .append(previous.fold("")(p => ", p=Z" + p.id))
        .append(")")
    sb.toString()
  }

}

object Task {
  val autoincrementId = new AtomicInteger()

  def apply(previous: Option[Task], delay: Int, time: Int, height: Int): Task = {
    apply(autoincrementId.getAndIncrement(), previous, delay, time, height)
  }

  def resetAutoincrementId() {
    autoincrementId.set(0)
  }
}
