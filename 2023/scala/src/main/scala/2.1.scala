package `2.2023.advent`

import cats.implicits._
import cats.data.NonEmptyList

import cats.effect._

import fs2._

import cats.parse.Parser._
import cats.parse.Rfc5234._
import cats.parse.Numbers._

object Day2Part1 extends IOApp.Simple {
  val bag = Map(
    "red"   -> BigInt(12),
    "green" -> BigInt(13),
    "blue"  -> BigInt(14)
  )

  val colors   = Set("red", "green", "blue")
  val rgb      = oneOf(colors.toList.map(string(_))).string
  val numColor = ((bigInt <* sp) ~ rgb).map { case (num, color) => color -> num }
  val cubes    = numColor.repSep(string(", "))
  val draws    = cubes.repSep(string("; ")).map(_.flatten)
  val game     = ((string("Game ") *> bigInt) <* string(": ")) ~ draws
  
  def drawLegal(color: String, number: BigInt): Boolean          = number <= bag(color)
  def drawsLegal(draws: NonEmptyList[(String, BigInt)]): Boolean = draws.forall(Function.tupled(drawLegal))

  def parseGame(line: String) = game.parse(line)
    .map(_._2)
    .leftMap(e => new RuntimeException(e.show))

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def run: IO[Unit] = {
    val banner = Stream.emit("Total of possible game IDs is ").covary[IO]
    val nl     = Stream.emit("\n").covary[IO]

    val input = io.readClassLoaderResource[IO]("2-input.txt")
      .through(text.utf8.decode)
      .through(text.lines)

    val result = input
      .evalMapFilter { line =>
        if (line.isBlank()) none[BigInt].pure[IO] else {
          val gameRep = parseGame(line)
          gameRep.map { case (id, draws) =>
            drawsLegal(draws)
            .guard[Option]
            .as(id)
          }.liftTo[IO]
        }
      }.foldMonoid

    val out =
      banner.through(io.stdoutLines()) ++
      result.through(io.stdoutLines()) ++
      nl.through(io.stdoutLines())

    out.compile.drain
  }
}
