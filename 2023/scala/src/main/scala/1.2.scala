package `1.2023.advent`

import cats._, cats.implicits._
import cats.effect._, cats.effect.implicits._

import fs2._

import cats.parse.Rfc5234._
import cats.parse.Parser._

object Part2 extends IOApp.Simple {
  val int   = digit.map(_.asDigit)
  val one   = string("one").as(1)
  val two   = string("two").as(2)
  val three = string("three").as(3)
  val four  = string("four").as(4)
  val five  = string("five").as(5)
  val six   = string("six").as(6)
  val seven = string("seven").as(7)
  val eight = string("eight").as(8)
  val nine  = string("nine").as(9)
  
  val number = int | one | two | three | four | five | six | seven | eight | nine
  val coord = (number.backtrack | alpha.as(0)).rep.map(_.filterNot(_ == 0))

  def parseCoordinate(line: String): Either[RuntimeException, Int] = {
    if (line.isBlank()) 0.asRight[RuntimeException] else {
      val num = coord.parse(line).map { case (_, vals) => vals match {
        case Nil      => 0
        case v :: Nil => v * 10 + v
        case h :: t   => h * 10 + t.last
      }}

      num.leftMap(e => new RuntimeException(e.show))
    }
  }

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
