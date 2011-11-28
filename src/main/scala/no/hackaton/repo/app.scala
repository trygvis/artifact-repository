package no.hackaton.repo

import unfiltered.filter._
import unfiltered.request._
import unfiltered.response._
import scalax.io._

class Urls(baseurl: UrlBuilder) {
  lazy val downloadArtifact: UrlBuilder = baseurl / "download"
}

object Urls {
  def unapply[T](req: HttpRequest[T]) = {
    val protocol = if(req.isSecure) "https" else "http"
    val (host, port) = HostPort.unapply(req).get
    val h = host.replaceAll("(\\S*):.*", "$1")
    val path = "/" // Path.unapply(req).get
    Some(new Urls(UrlBuilder(protocol, h, port, path, Params.unapply(req).get)))
  }
}

trait MavenDefinedKeys {
  def groupId = "group-id"
  def artifactId = "artifact-id"
  def buildNumber = "build-number"
}

object DefinedKeys extends MavenDefinedKeys {
  def directDownload = "direct-download"
  def timestamp = "timestamp"
}

object Want {
  def wantFromQuery[T](req: HttpRequest[T]) = {
      val x: Option[String] = Params.unapply(req).get("mediaType").headOption
      x.flatMap {
      case "text/plain" => Some(WantPlainText)
      case "application/atom+xml" => Some(WantAtom)
      case _ => None
    }
  }

  def wantFromAccept[T](req: HttpRequest[T]) = Accept(req) match {
    case l@List(_) if l.contains("text/plain") => Some(WantPlainText)
    case l@List(_) if l.contains("application/atom+xml") => Some(WantAtom)
    case _ => None
  }

  def unapply[T](req: HttpRequest[T]) = wantFromQuery(req).orElse(wantFromAccept(req))
}

sealed trait Want
case object WantAtom extends Want
case object WantPlainText extends Want

class ArtifactRepositoryPlan(db: ArtifactDatabase) extends Plan {
  def paramsToFilter(params: Map[String, Seq[String]]) = {
    val p = params.mapValues(_.last).filter(!_._2.isEmpty)
    println("params=" + p)
    Artifact.artifactFilter(p) _
  }

  import HtmlTemplates._
  def intent = {
    case Path(Seg(Nil)) =>
      Redirect("/index.html")
    case Path(Seg("index.html" :: Nil)) & Urls(urls) =>
      Html(main(frontpage(urls)))
    case req@Path(Seg("dev" :: "null" :: Nil)) & Urls(urls) =>
      println("Eating " + req.uri + ", yum!")
      Ok ~> ResponseString("Sucker!")
    case req@Path("/download") & Params(params) =>
      val p = params.mapValues(_.last).filter(!_._2.isEmpty)
      val seq = db.find(paramsToFilter(params))
      Want.unapply(req) match {
        case Some(WantAtom) =>
          Ok ~> ResponseString("atom")
        case Some(WantPlainText) =>
          // TODO: This is up to the response media type, some might want to return an empty list instead
          Ok ~> (seq match {
            case Seq() => PlainTextNotFound(p)
            case Seq(value) => PlainTextSingleArtifact(p, value)
            case Seq(values @ _*) => PlainTextMultipleArtifacts(p, values)
          })
        case _ =>
          Ok ~> ResponseString("Unknown content type\n")
      }
    case req@Path("/upload") & Params(params) =>
      // TODO: Implement support for file uploads (www-urlencoded or whatever it is)
      println("Storing " + params.mapValues {_.last })
      val stream = Body.stream(req)
      val attributes = params.mapValues(_.head)
      val uuid = db.save(attributes, Resource.fromInputStream(stream))
      Created ~> ResponseString("uuid = " + uuid + "\n")
    case Path(Seg(p :: Nil)) =>
      NotFound ~> ResponseString(p + "\n")
  }
}

object ArtifactRepositoryApp extends App {
  val basedir = scalax.file.Path("artifact-db")
  basedir.createDirectory(true, false)
  val db = new ArtifactDatabase(basedir)
  val artifactRepositoryPlan = new ArtifactRepositoryPlan(db)

  val mavenRepositoryPlan = new MavenRepositoryPlan

  unfiltered.jetty.Http(8080).
    filter(mavenRepositoryPlan).
    filter(artifactRepositoryPlan).
    run()
}
