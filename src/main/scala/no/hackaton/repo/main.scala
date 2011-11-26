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

class ArtifactRepositoryPlan(db: ArtifactDatabase) extends Plan {
  def paramsToFilter(params: Map[String, Seq[String]]) = (artifact: Artifact) => {
    val p = params.mapValues(_.last)

    true
  }

  import HtmlTemplates._
  def intent = {
    case Path(Seg(Nil)) =>
      Redirect("/index.html")
    case Path(Seg("index.html" :: Nil)) & Urls(urls) =>
      Html(main(frontpage(urls)))
    case Path(Seg("dev" :: "null" :: Nil)) & Urls(urls) =>
      Ok ~> ResponseString("Sucker!")
    case Path("/download") & Params(params) =>
      val seq = db.find(paramsToFilter(params))
        // TODO: This is up to the response media type, some might want to return an empty list instead
      seq match {
        case Seq() =>
          NotFound ~> ResponseString("Could not find any results matching: " + params)
        case Seq(value) =>
          Ok ~> ResponseString("Found single item " + value + "\n")
        case Seq(values @ _*) =>
          Ok ~> ResponseString("Found many values item " + values + "\n")
      }
    case req@Path("/upload") & Params(params) =>
      // TODO: Implement support for file uploads (www-urlencoded or whatever it is)
      ResponseString("Uploading " + params)
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
