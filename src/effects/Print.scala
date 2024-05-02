package effects

import cats.effect.{IO, IOApp}

/**
 * Scala 2 translation of `Print` effect example from [[https://noelwelsh.com/posts/direct-style/]]
 */
trait Print[A] {
  def apply()(implicit c: Console.type): A
}
object Print {
  type Console = Console.type

  def print(msg: Any): Print[Unit] = apply(_.print(msg))

  def println(msg: Any): Print[Unit] = apply(_.println(msg))

  def run[A](print: Print[A]): A = print()(Console)

  def apply[A](body: Console => A): Print[A] = new Print[A] {
    override def apply()(implicit c: Console): A = body(c)
  }

  implicit final class PrintOps[A](private val print: Print[A]) extends AnyVal {
    def prefix[B](first: Print[B]): Print[A] = apply { implicit c =>
      first()
      print()
    }

    def red: Print[A] = apply { implicit c =>
      Print.print(Console.RED)()
      val result = print()
      Print.print(Console.RESET)()
      result
    }
  }
}

object Main extends IOApp.Simple {
  override def run: IO[Unit] = IO {
    val message = Print.println("Hello from direct-style land!")
    val red =
      Print.println("Amazing!").prefix(Print.print("> ").red)

    Print.run(message)
    Print.run(red)
  }
}