package no.hackaton.repo

import unfiltered.filter._
import unfiltered.response._
import java.util.Date

object ResponseUtils {
  def queryToString(params: Map[String, String]) =
    params.map(kv => kv._1 + "=" + kv._2).mkString(", ")
}

import ResponseUtils._

object PlainTextNotFound {
  def apply(params: Map[String, String]) = ResponseString("Could not find any results matching: " + params + "\n")
}

object PlainTextSingleArtifact {
  def apply(params: Map[String, String], value: Artifact) = {
    val s = "Query = " + queryToString(params) + ", \n" +
      "Artifact: " + value.attributes.toList.map(t => t._1 + " = " + t._2).mkString("\n") + "\n"
    Charset(utf8Charset) ~> PlainTextContent ~> ResponseString(s)
  }
}

object PlainTextMultipleArtifacts {
  def apply(params: Map[String, String], values: Seq[Artifact]) = {
    val s = "Query = " + params + ", \n" +
    "Found " + values.size + " artifacts.\n"
    Charset(utf8Charset) ~> PlainTextContent ~> ResponseString(s + values.map(_.lines.sorted.mkString("\n")).map("\n" + _).mkString("\n") + "\n")
  }
}

object AtomFeed {
  import atom._
  def apply(date: Date, params: Map[String, String], values: Seq[Artifact])(implicit urls: Urls) = {
    val author = Person("Artifact Repository")
    val s = Feed(
      "urn:artifact-repository:search",
      "Query: " + queryToString(params),
      date,
      Seq(author),
      values.map(artifact2entry)).toXml.toString
    Charset(utf8Charset) ~> ContentType("application/atom+xml") ~> ResponseString(s)
  }

  def artifact2entry(a: Artifact)(implicit urls: Urls) = {
    val links = List(
        Link(urls.artifact(a.attributes)(), Some(WantAtom.mediaType), Some("alternate")),
        Link(urls.artifact(a.attributes)(), Some(WantHtml.mediaType), Some("alternate")))
    Entry(a.title, a.urn, a.timestamp, links)
  }
}
