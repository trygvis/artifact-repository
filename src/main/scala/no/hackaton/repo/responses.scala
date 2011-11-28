package no.hackaton.repo

import unfiltered.filter._
import unfiltered.response._

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
