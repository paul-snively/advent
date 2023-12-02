package `2.2023.advent`

import cats.implicits._
import cats.data.NonEmptyList

import cats.effect._

import fs2._

import cats.parse.Parser._
import cats.parse.Rfc5234._
import cats.parse.Numbers._

object Day2Part2 extends IOApp.Simple {
  val colors   = Set("red", "green", "blue")
  val rgb      = oneOf(colors.toList.map(string(_))).string
  val numColor = ((bigInt <* sp) ~ rgb).map { case (num, color) => color -> num }
  val cubes    = numColor.repSep(string(", "))
  val draws    = cubes.repSep(string("; ")).map(_.flatten)
  val game     = ((string("Game ") *> bigInt) <* string(": ")) ~ draws

  def parseGame(line: String): Either[RuntimeException, (BigInt, NonEmptyList[(String, BigInt)])] =
    game.parse(line)
    .map(_._2)
    .leftMap(e => new RuntimeException(e.show))

  def smallestBag(draws: NonEmptyList[(String, BigInt)]): Map[String, BigInt] = {
    val root = Map.empty[String, BigInt].withDefaultValue(BigInt(0))
    val grouped = draws.groupBy(_._1).view.mapValues(_.map(_._2).maximum)
    root ++ grouped
  }

  def power(min: Map[String, BigInt]): BigInt = min.values.fold(BigInt(1))(_ * _)
  
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def run: IO[Unit] = {
    val banner = Stream.emit("Total of power of minimum bags is ").covary[IO]
    val nl     = Stream.emit("\n").covary[IO]

    val input = io.readClassLoaderResource[IO]("2-input.txt")
      .through(text.utf8.decode)
      .through(text.lines)

    val result = input
      .evalMap { line =>
        if (line.isBlank()) BigInt(0).pure[IO] else {
          val gameRep = parseGame(line)
          gameRep.map { case (_, draws) => {
            power(smallestBag(draws))
          }}.liftTo[IO]
        }
      }.foldMonoid

    val out =
      banner.through(io.stdoutLines()) ++
      result.through(io.stdoutLines()) ++
      nl.through(io.stdoutLines())

    out.compile.drain
  }
}
