package no.hackaton.repo

import unfiltered.filter._
import unfiltered.response._
import java.util.Date

trait ResponseMediaType {
  def contentType: String
}

object PlainTextNotFound {
  def apply(params: Map[String, String]) = ResponseString("Could not find any results matching: " + params + "\n")
}

object PlainTextSingleArtifact {
  def apply(params: Map[String, String], value: Artifact) = {
    val s = "Query = " + params + ", \n" +
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
  def apply(date: Date, params: Map[String, String], values: Seq[Artifact]) = {
    val author = Person("Artifact Repository")
    val s = Feed(
      "urn:artifact-repository:search",
      "Query: " + params.map(kv => kv._1 + "=" + kv._2).mkString(", "),
      date,
      Some(author),
      values.map(a => Entry(a.title, a.urn, a.timestamp))).toXml.toString
    Charset(utf8Charset) ~> ContentType("application/atom+xml") ~> ResponseString(s)
  }
}
