package effects.builders

import cats.syntax.all._
import cats.effect.{IO, IOApp}
import io.circe.{Encoder, Json}

import scala.collection.mutable.ListBuffer

/**
 * Example of fluent builder
 */
object FluentJsonBuilder extends IOApp.Simple {
  object Builder {
    sealed trait Buildable {
      private[Builder] def result: Json
    }

    object Base {
      def obj: Buildable = obj(identity)
      def obj(f: ObjectBuilder => ObjectBuilder): Buildable = f(new ObjectBuilder(ListBuffer.empty))

      def array: Buildable = array(identity)
      def array(f: ArrayBuilder => ArrayBuilder): Buildable = f(new ArrayBuilder(ListBuffer.empty))

      def value[A: Encoder](value: A): Buildable = new ScalarBuilder(Encoder[A].apply(value))
    }

    type Base = Base.type

    class ObjectBuilder(private[Builder] val fields: ListBuffer[(String, Json)])
      extends Buildable {
      def add(key: String)(f: Base => Buildable): ObjectBuilder =
        new ObjectBuilder(fields.addOne(key -> f(Base).result))

      override private[Builder] def result: Json = Json.fromFields(fields)
    }

    class ArrayBuilder(private[Builder] val elements: ListBuffer[Json])
      extends Buildable {
      def add(f: Base => Buildable): ArrayBuilder =
        new ArrayBuilder(elements.addOne(f(Base).result))

      override private[Builder] def result: Json = Json.fromValues(elements)
    }

    class ScalarBuilder(private[Builder] val result: Json) extends Buildable

    def build(f: Base => Buildable): Json = f(Base).result
  }

  override def run: IO[Unit] = {
    val json = Builder.build(_
      .obj(_
        .add("foo")(_.value(1))
        .add("bar")(_.array(_
          .add(_.value(1))
          .add(_.value(true))
          .add(_.obj(_
            .add("baz")(_.obj)
          ))
        )))
    )

    IO.consoleForIO.println(json) >>
      IO.unlessA(json === JsonFactory.canonical) {
        IO.consoleForIO.println(show"Not equal to ${JsonFactory.canonical}")
      }
  }
}