package `3.2023.advent`

import scala.util.Try

import cats.implicits._

import cats.effect._

import fs2._

import cats.parse.Parser._
import cats.parse.Numbers._

object Day3Part1 extends IOApp.Simple {
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def extractPartNumbers(incoming: Chunk[String]): Stream[IO, BigInt] = {
    // Pad incoming Chunk if it's less than 3 lines or has blank ones
    val clean = incoming.filterNot(_.isBlank)
    val block = clean ++ Chunk.from(List.fill(3 - clean.size)("." * incoming(0).length))
    
    def t(exp: => Boolean) = Try(exp).toOption.getOrElse(false)

    def neighborSlice(index: Int)(begin: Int, end: Int): String = {
      val neighbor = block(index)
      neighbor.substring(begin, end)
    }

    val splitNum = anyChar.repUntil0(digit.rep).string ~ digit.rep.string.?

    def parser(input: String): IO[(String, (String, Option[BigInt]))] = splitNum.parse(input)
      .bimap(
        e => new RuntimeException(e.show),
        { case (remainder, (soFar, so)) => (remainder, (soFar, so.map(BigInt.apply))) }

      )
      .liftTo[IO]

    def check(num: BigInt, soFar: String, remainder: String): Boolean = {
      val numSize       = num.show.length
      val consumed      = soFar.length
      val begin         = if (consumed > 0) consumed - 1 else consumed
      val end           = consumed + numSize + (if (remainder.isEmpty) 0 else 1)
      val above         = neighborSlice(0)(begin, end)
      val below         = neighborSlice(2)(begin, end)
      val aboveNeighbor = above.exists(c => !(c == '.' || c.isDigit))
      val belowNeighbor = below.exists(c => !(c == '.' || c.isDigit))
      val leftNeighbor  = t(!(soFar.last == '.' || soFar.last.isDigit))
      val rightNeighbor = t(remainder.head != '.')
      val partNumber = aboveNeighbor || belowNeighbor || leftNeighbor || rightNeighbor
      partNumber
    }

    /* Consider the middle of 3 lines and symbols in lines above and below.
     * Amusingly, `parse` gives us the stuff before the number, because we asked for it,
     * and the remainder to be parsed, because that's how `parse` vs. `parseAll` works.
     */

    // Build a Stream from what's left to parse (everything) and what's been seen so far (nothing)
    val out = Stream.unfoldEval((block(1), "")) { case (toParse, seen) =>
      if (toParse.isEmpty) {
        none[(Chunk[BigInt], (String, String))].pure[IO]       // We're done here
      } else {
        for {
          parsed <- parser(toParse)
          (remainder, (alphas, numO)) = parsed
          soFar   = seen ++ alphas
          o       = numO.fold(Chunk.empty[BigInt]) { num =>
            if (check(num, soFar, remainder)) Chunk.singleton(num) else Chunk.empty[BigInt]
          }
          // Emit o and continue with what's left to parse and ALL of what's been seen
          result  = Option((o, (remainder, soFar ++ (numO.fold("")(_.show)))))
        } yield result
      }
    }
    
    out
      .foldMonoid                          // Fold part numbers into one Chunk
      .flatMap(Stream.chunk)               // Unchunk
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def process(input: Stream[IO, String]): Stream[IO, BigInt] = {
    input.sliding(3)                                           // 1 line above the one considered, 1 below
      .flatMap {
        (extractPartNumbers _)
      }
      .foldMonoid                                              // Add up part number totals from all blocks
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def run: IO[Unit] = {
    val banner = Stream.emit("The total of all part numbers is ").covary[IO]
    val nl     = Stream.emit("\n").covary[IO]
    val input = io.readClassLoaderResource[IO]("3-input.txt")
      .through(text.utf8.decode)
      .through(text.lines)

    // Pulls can only go back to Streams when O is Unit, so wrap all the Stream processing here
    input.pull.peek1.evalMap { op =>
      val result = for {
        (line, lines) <- Stream.eval(IO.fromOption(op)(new RuntimeException("The input is empty.")))
        // We peeked so we could put a no-part-number-or-symbol line of the right length in front
        prep = Stream.emit("." * line.length) ++ lines
        total         <- process(prep)
        _             <- (banner ++ Stream.emit(total.show) ++ nl).through(io.stdoutLines()).void
       } yield ()

      result.compile.drain               // Sledgehammer our Stream to an IO[Unit]
    }.stream.compile.drain
  }
}
