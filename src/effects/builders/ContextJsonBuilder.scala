package effects.builders

import cats.syntax.all._
import cats.effect.{IO, IOApp}
import io.circe.{Encoder, Json}
import io.circe.syntax._

import scala.collection.mutable.ListBuffer

/**
 * Scala 2 translation of using Context Functions to implement a builder
 */
object ContextJsonBuilder extends IOApp.Simple {
  object Builder {
    class ObjectBuilder {
      private [Builder] val fields: ListBuffer[(String, Json)] = ListBuffer.empty
    }
    class ArrayBuilder {
      private [Builder] val elements: ListBuffer[Json] = ListBuffer.empty
    }
    object syntax {
      def obj(f: ObjectBuilder => Unit): Json = {
        val builder = new ObjectBuilder
        f(builder)
        Json.fromFields(builder.fields)
      }

      def array(f: ArrayBuilder => Unit): Json = {
        val builder = new ArrayBuilder
        f(builder)
        Json.fromValues(builder.elements)
      }

      def add(value: (String, Json))(implicit obj: ObjectBuilder): Unit =
        obj.fields.addOne(value)

      def add[A: Encoder](value: A)(implicit arr: ArrayBuilder): Unit =
        arr.elements.addOne(Encoder[A].apply(value))
    }
  }

  override def run: IO[Unit] = {
    import Builder.syntax._
    val json = obj { implicit b =>
      add("foo" := 1)
      add("bar" -> array { implicit b =>
        add(1)
        add(true)
        add(obj { implicit b =>
          add("baz" -> obj(_ => ()))
        })
      })
    }

    IO.consoleForIO.println(json) >>
      IO.unlessA(json === JsonFactory.canonical) {
        IO.consoleForIO.println(show"Not equal to ${JsonFactory.canonical}")
      }
  }
}