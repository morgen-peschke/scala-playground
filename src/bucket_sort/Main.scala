package bucket

import cats.Show
import cats.effect.std.Random
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import com.monovore.decline.{Command, Opts}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.NoStackTrace

final case class Arguments(elementCount: Int, min: Int, max: Int, bucketCount: Int)
object Arguments {
  val command: Command[Arguments] =
    Command[Arguments](
      name = "bucket_sort.Main",
      header = "BucketSort exploration"
    ) {
      (
        Opts
          .option[Int]("size", help = "Size of test list")
          .validate("--size cannot be negative")(_ >= 0),
        Opts.option[Int]("min", help = "Inclusive lower bound for generated numbers").withDefault(0),
        Opts.option[Int]("max", help = "Exclusive upper bound for generated numbers"),
        Opts
          .option[Int]("buckets", help = "Number of buckets")
          .validate("--buckets cannot be less than 1")(_ > 0),
      ).mapN(Arguments.apply).validate("--min must be less than or equal to --max") { a =>
        a.min <= a.max
      }
    }
}

/**
 * Playing around with BucketSort
 * @see [[https://en.wikipedia.org/wiki/Bucket_sort]]
 */
object Main extends IOApp {
  private val console = IO.consoleForIO
  private implicit def showListInt[F[_] <: Seq[_]]: Show[F[Int]] = Show.show(_.mkString("[", ",", "]"))

  private final class SnapShot(buckets: Vector[Vector[Int]], remaining: List[Int], maxNum: Int) {
    override def toString: String = {
      val step = buckets.foldLeft(0)(_ + _.size)
      val maxWidth = maxNum.toString.length
      val missingCell = " " * maxWidth
      val maxIndex = buckets.map(_.size).maximumOption.getOrElse(1).max(1)
      val rendered = buckets.map {
        _.map(_.toString.padTo(maxWidth, ' ')).padTo(maxIndex, missingCell)
      }
      val maxSizeWidth = (step + remaining.size).toString.length
      val stepStr = s"$step".padTo(maxSizeWidth, ' ')
      val buffer = new StringBuilder()
      buffer
        .append("Step ")
        .append(stepStr)
        .append(" [")

      val padding = " " * buffer.length

      buffer.append(rendered.map(_.get(0).getOrElse(" ")).mkString(s"]["))
      if (remaining.isEmpty) {
        buffer.append("]\n")
      }
      else {
        buffer
          .append("] <- ")
          .append(remaining.mkString(" "))
          .append('\n')
      }

      (1 until maxIndex).foreach { row =>
        buffer
          .append(padding)
          .append(rendered.map(_.get(row).getOrElse(missingCell)).mkString("  "))
          .append('\n')
      }
      buffer.result().stripTrailing()
    }
  }
  private object SnapShot {
    def apply(buckets: Vector[mutable.ReusableBuilder[Int, Vector[Int]]], remaining: List[Int], max: Int): SnapShot =
      new SnapShot(buckets.map(_.result()), remaining, max)
  }

  def bucketSort(nums: Seq[Int], bucketCount: Int): Seq[Int] =
    if (nums.length <= 1) nums
    else {
      val buckets = Vector.fill(bucketCount)(Vector.newBuilder[Int])
      val max = nums.maximumOption.getOrElse(0)
      val scaling = (bucketCount - 1).toDouble / max.toDouble

      @tailrec
      def loop(remaining: List[Int]): Vector[Vector[Int]] =
        remaining match {
          case Nil => buckets.map(_.result())
          case num :: remaining =>
            val index = (num * scaling).floor.toInt
            buckets(index).addOne(num)
            println(SnapShot(buckets, remaining, max))
            loop(remaining)
        }

      val input = nums.toList
      println(SnapShot(buckets, input, max))
      loop(input).flatMap(_.sorted)
    }


  override def run(args: List[String]): IO[ExitCode] =
    Arguments.command.parse(args) match {
      case Left(help) =>
        console.errorln(help).as(ExitCode.Error)
      case Right(Arguments(elementCount, min, max, bucketCount)) =>
        for {
          random <- Random.scalaUtilRandom[IO]
          input <- (0 until elementCount).toList.traverse(_ => random.nextIntBounded(max - min).map(_ + min))
          _ <- console.print("Randomized: ") >> console.println(input)
          sorted = bucketSort(input, bucketCount)
          _ <- console.print("Sorted: ") >> console.println(sorted)
        } yield ExitCode.Success
    }
}