package `1.2023.advent`

import cats.implicits._
import cats.effect._

import fs2._

import cats.parse.Rfc5234._

object Day1Part1 extends IOApp.Simple {
  val nonDigit   = alpha.rep0
  val firstDigit = digit.surroundedBy(nonDigit).map(_.asDigit)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def parseCoordinate(line: String): Either[RuntimeException, Int] = {
    if (line.isBlank()) 0.asRight[RuntimeException] else {
      val first  = firstDigit.parse(line).map(_._2 * 10)
      val second = firstDigit.parse(line.reverse).map(_._2)

      val either = (first, second).mapN { case (a, b) => a + b }
      either.leftMap(e => new RuntimeException(e.show))
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
