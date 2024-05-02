package effects.builders

import cats.effect.{IO, IOApp}
import io.circe.Json
import io.circe.syntax._

/**
 * Example of a builder that's actually a bunch of factory functions (provided by `circe`)
 */
object JsonFactory extends IOApp.Simple {
  val canonical: Json =
    Json.obj(
      "foo" := 1,
      "bar" -> Json.arr(
        1.asJson,
        true.asJson,
        Json.obj("baz" -> Json.obj())
      )
    )

  override def run: IO[Unit] = IO.consoleForIO.println(canonical)
}