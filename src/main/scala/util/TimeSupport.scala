package util

import java.util.TimeZone

import org.joda.time.DateTimeZone

import scala.util.Try

object TimeSupport {
  def useUtc(): Unit = {
    DateTimeZone.setDefault(DateTimeZone.forID("UTC"))
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"))
  }

  // StopWatch
  def stopWatch[A](process: => A, task: String=""): A = {
    val start = System.currentTimeMillis()
    val result = Try(process)
    val stop = System.currentTimeMillis()
    val timeDiff = stop - start

    lazy val hours = (timeDiff / (1000 * 60 * 60)) % 24
    lazy val minutes = (timeDiff / (1000 * 60)) % 60
    lazy val seconds = (timeDiff / 1000) % 60
    lazy val milliseconds = timeDiff / 1000

    // format time diff in HH:MM:SS:MMM
    lazy val elapsedTime = "%02d:%02d:%02d.%03d".format(hours, minutes, seconds, milliseconds)

    println(s"Task: $task Time: $elapsedTime")
    result.get
  }

}
