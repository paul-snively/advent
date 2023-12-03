package `1.2023.advent`

import cats.implicits._
import cats.effect._

import fs2._

import cats.parse.Rfc5234._
import cats.parse.Parser._

object Day1Part2 extends IOApp.Simple {
  val int   = digit.map(d => List(d.asDigit))
  val oneht = string("oneight").as(List(1, 8))
  val twone = string("twone").as(List(2, 1))
  val theht = string("threeight").as(List(3, 8))
  val feht  = string("fiveight").as(List(5, 8))
  val senin = string("sevenine").as(List(7, 9))
  val ehtto = string("eightwo").as(List(8, 2))
  val ehtth = string("eighthree").as(List(8, 3))
  val nneht = string("nineight").as(List(9, 8))
  val one   = string("one").as(List(1))
  val two   = string("two").as(List(2))
  val three = string("three").as(List(3))
  val four  = string("four").as(List(4))
  val five  = string("five").as(List(5))
  val six   = string("six").as(List(6))
  val seven = string("seven").as(List(7))
  val eight = string("eight").as(List(8))
  val nine  = string("nine").as(List(9))
 
  val number = 
    int   |
    oneht |
    twone |
    theht |
    feht  |
    senin |
    ehtto |
    ehtth |
    nneht |
    one   |
    two   |
    three |
    four  |
    five  |
    six   |
    seven |
    eight |
    nine
  val coord  = number.surroundedBy(alpha.repUntil0(number)).rep0.map(_.flatten)
  
  // Suppressing IterableOps here is warranted by pattern-matching on the non-empty List case.
  @SuppressWarnings(Array("org.wartremover.warts.IterableOps"))
  def parseCoordinate(line: String): Either[RuntimeException, Int] = {
    if (line.isBlank()) 0.asRight[RuntimeException] else {
      val num = coord.parse(line).map { case (_, vs) => vs match {
        case Nil      => 0
        case v :: Nil => v * 10 + v
        case h :: t   => h * 10 + t.last
      }}

      num.leftMap(e => new RuntimeException(e.show))
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def run: IO[Unit] = {
    val banner = Stream.emit("Resulting total coordinate: ").covary[IO]
    val nl     = Stream.emit("\n").covary[IO]
    val input  = io.readClassLoaderResource[IO]("1-input.txt")
      .through(text.utf8.decode)
      .through(text.lines)
    val result = input.evalMap(parseCoordinate(_).liftTo[IO]).foldMonoid

    val out = banner.through(io.stdoutLines()) ++
              result.through(io.stdoutLines()) ++
              nl.through(io.stdoutLines())

    out.compile.drain
  }
}
