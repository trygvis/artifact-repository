package no.hackaton.repo

import java.util.{Date, UUID}
import scala.collection.immutable.TreeMap
import scalax.io._
import scalax.file._

case class Artifact(path: Path, timestamp: Date, attributes: Attributes) {
  def title = attributes.map(kv => kv._1 + "=" + kv._2).mkString(",")
  def urn = "urn:artifact:" + title

  def lines = Artifact.attributesToLines(attributes)
}

object Artifact {
  private lazy val emptyAttributes = TreeMap.empty[String, String]

  def attributesToLines(attributes: Attributes) = (List.empty[String] /: attributes)((list, attribute) => (attribute._1 + " = " + attribute._2) :: list)

  def load(path: Path): Artifact = {
    apply(path, Resource.fromFile(path.path).reader.lines().toList)
  }

//  def apply(path: Path, lines: String*): Artifact = apply(path, lines.toSeq)

  def apply(path: Path, lines: Seq[String]) = {
    val Attribute = """(.*) = (.*)""".r
    def toKV(line: String) = line match {
      case Attribute(k, v) => Some((k, v))
      case _ => None
    }
    new Artifact(path, new Date(path.lastModified), emptyAttributes ++ lines.flatMap(toKV))
  }

  def artifactFilter(params: Map[String, String])(artifact: Artifact) = {
    val as = artifact.attributes
    var ok = true
//    println("artifact: " + artifact)
    params.filter({ case (key, expectedValue) =>
      as.get(key) match {
        case Some(value) if value.equals(expectedValue) => false
        case _ => true
      }
    }).isEmpty
  }
}

class ArtifactDatabase(val dir: Path) {
  def find(filter: Artifact => Boolean): ArtifactSeq = {
    val files = dir * PathMatcherFactory.GlobToMatcher("*.db")

    files.map(Artifact.load).filter(filter).toSeq
  }

  def save(attributes: Attributes, input: Input): UUID = {
    val uuid = UUID.randomUUID
    println("ArtifactDatabase.save: uuid=" + uuid)
    val dbTmp = dir / (uuid.toString + ".db.tmp")
    val db = dir / (uuid.toString + ".db")
    val blobTmp = dir / (uuid.toString + ".blob.tmp")
    val blob = dir / (uuid.toString + ".blob")

    Resource.fromFile(dbTmp.path).writer.write(Artifact.attributesToLines(attributes).mkString("\n") + "\n")
    input.copyDataTo(Resource.fromFile(blobTmp.path))

    blobTmp.moveTo(blob, true, true)
    dbTmp.moveTo(db, true, true)

    uuid
  }
}
