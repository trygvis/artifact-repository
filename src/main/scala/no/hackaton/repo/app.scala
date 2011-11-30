package no.hackaton.repo

import unfiltered.filter._
import unfiltered.request._
import unfiltered.response._
import java.util.{Date => JDate}
import scala.collection.immutable.{TreeMap,TreeSet}
import scalax.io._

class Urls(private val baseurl: UrlBuilder) {
  def start = baseurl / "index.html"

  def search: UrlBuilder = search(Map.empty)

  def search(params: Map[String, String], mediaType: Option[Want] = None) =
    addMediaType((baseurl / "artifact") ? params, mediaType)

  def artifact(params: Map[String, String], mediaType: Option[Want] = None) =
    addMediaType((baseurl / "artifact") ? params, mediaType)

  private def addMediaType(url: UrlBuilder, mediaType: Option[Want]) =
    mediaType.flatMap(want => Some(url ? (DefinedKeys.mediaType -> want.id))).getOrElse(url)
}

object Urls {
  def unapply[T](req: HttpRequest[T]) = {
    val protocol = if(req.isSecure) "https" else "http"
    val (host, port) = HostPort.unapply(req).get
    val h = host.replaceAll("(\\S*):.*", "$1")
    val path = "/"
    val query = Map.empty[String, Seq[String]] // Params.unapply(req).get
    Some(new Urls(UrlBuilder(protocol, h, port, path, query)))
  }
}

trait MavenDefinedKeys {
  def groupId = "group-id"
  def artifactId = "artifact-id"
  def buildNumber = "build-number"
}

object DefinedKeys extends MavenDefinedKeys {
  private var keySet = TreeSet.empty[String]
  private def key(key: String) = { keySet = keySet + key; key }

  def keys: Set[String] = keySet

  val directDownload = key("direct-download")
  val timestamp = key("timestamp")
  val mediaType = key("media-type")
}

sealed class Want(val id: String, val mediaType: String)
case object WantAtom extends Want("atom", "application/atom+xml")
case object WantHtml extends Want("html", "text/html")
case object WantPlainText extends Want("text", "text/plain")

object Want {
  import DefinedKeys._

  def wantFromQuery[T](req: HttpRequest[T]) = {
    Params.unapply(req).get(mediaType).headOption.flatMap {
      case WantPlainText.id => Some(WantPlainText)
      case WantAtom.id => Some(WantAtom)
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

// xml = <?xml version=\"1.0\" encoding=\"utf-8\"?>\n
case class Html5(nodes: scala.xml.NodeSeq) extends ComposeResponse(HtmlContent ~> ResponseString("<!DOCTYPE html>\n" + scala.xml.Utility.toXML(nodes.head, minimizeTags = true)))

class ArtifactRepositoryPlan(db: ArtifactDatabase) extends Plan {
  import HtmlTemplates._
  def intent = {
    case Path(Seg(Nil)) =>
      Redirect("/index.html")
    case Path(Seg("index.html" :: Nil)) & Urls(urls) =>
      Html5(frontpage(urls))
    case req@Path(Seg("dev" :: "null" :: Nil)) =>
      println("Eating " + req.uri + ", yum!")
      Ok ~> ResponseString("Sucker!")
    case req@Path("/search") & Params(params) & Urls(urls) =>
      val date = new JDate
      val p = params.mapValues(_.last).filter(!_._2.isEmpty) -- DefinedKeys.keys
      println("Artifact filter: " + p)
      lazy val seq = db.find(Artifact.artifactFilter(p))

//      Date(date) ~> // TODO: Set this to HTTP date
      Want.unapply(req) match {
        case Some(WantAtom) =>
          Ok ~> AtomFeed(date, p, seq)(urls)
        case Some(WantPlainText) =>
          // TODO: This is up to the response media type, some might want to return an empty list instead
          Ok ~> (seq match {
            case Seq() => PlainTextNotFound(p)
            case Seq(value) => PlainTextSingleArtifact(p, value)
            case Seq(values @ _*) => PlainTextMultipleArtifacts(p, values)
          })
        case _ =>
          Ok ~> Html5(searchResult(urls, p, seq))
      }
    case req@Path("/upload") & Params(params) =>
      // TODO: Implement support for file uploads (www-urlencoded or whatever it is)
      println("Storing " + params.mapValues {_.last })
      val stream = Body.stream(req)
      val attributes = TreeMap.empty[String, String] ++ (params.mapValues(_.head))
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

  unfiltered.jetty.Http(4460).
    filter(mavenRepositoryPlan).
    filter(artifactRepositoryPlan).
    run()
}
