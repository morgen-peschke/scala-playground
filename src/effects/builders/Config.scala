package context_builder

import cats.effect.{IO, IOApp}
import cats.syntax.all._
import org.http4s.Uri
import org.http4s.Uri.Path.Root
import org.http4s.Uri.{Host, Path, Scheme}

/**
 * Example of a builder with compile-time validation and auto-complete help
 */
sealed trait Config {
  def host: Host
  def port: Option[Int]
  def path: Path

  def render: String = Uri(
    scheme = Scheme.http.some,
    authority = Uri.Authority(host = host, port = port).some,
    path = path
  ).renderString
}
object Config {
  def builder: Builder[None.type, None.type, None.type] = new Builder(None, None, None)

  class Builder[
    H <: Option[Host],
    I <: Option[Int],
    P <: Option[Path]
  ] private[Config](
                     private [Config] val hostOpt: H,
                     private [Config] val portOpt: I,
                     private [Config] val pathOpt: P
  ) {
    private [Config] def withHost(host: Host): Builder[Some[Host], I, P] = new Builder(Some(host), portOpt, pathOpt)
    private [Config] def withPort(port: Int): Builder[H, Some[Int], P] = new Builder(hostOpt, Some(port), pathOpt)
    private [Config] def withPath(path: Path): Builder[H, I, Some[Path]] = new Builder(hostOpt, portOpt, Some(path))
  }

  implicit final class BuildOps[
    I <: Option[Int],
  ](private val builder: Builder[Some[Host],I,Some[Path]]) extends AnyVal {
    def build: Config = new Config {
      override val host: Host = builder.hostOpt.value
      override val port: Option[Int] = builder.portOpt
      override val path: Path = builder.pathOpt.value
    }
  }

  implicit final class SetHostOps[
    I <: Option[Int],
    P <: Option[Path]
  ](private val builder: Builder[None.type,I,P]) extends AnyVal {
    def host(host: Host): Builder[Some[Host], I, P] = builder.withHost(host)
  }

  implicit final class SetPortOps[
    H <: Option[Host],
    P <: Option[Path]
  ](private val builder: Builder[H,None.type,P]) extends AnyVal {
    def port(port: Int): Builder[H, Some[Int], P] = builder.withPort(port)
  }

  implicit final class SetPathOps[
    H <: Option[Host],
    I <: Option[Int]
  ](private val builder: Builder[H,I,None.type]) extends AnyVal {
    def path(path: Path): Builder[H, I, Some[Path]] = builder.withPath(path)
  }
}

object BuilderMain extends IOApp.Simple {
  private val localhost = Host.unsafeFromString("localhost")

  override def run: IO[Unit] = IO.consoleForIO.println {
    Config
      .builder
      .host(localhost)
      .path(Root / "foo" / "bar" / "baz")
      .build
      .render
  }
}