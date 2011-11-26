package no.hackaton.repo

//import java.io._
import java.util.UUID
import scalax.io._
import scalax.file._

case class Artifact(attributes: Attributes, path: Path) {
//  def fileBody = ("" /: attributes)((s, attribute) => s + attribute._1 + " = " + attribute._2 + "\n")

  def lines = Artifact.attributesToLines(attributes)
}

object Artifact {
  def attributesToLines(attributes: Attributes) = (List.empty[String] /: attributes)((list, attribute) => (attribute._1 + " = " + attribute._2) :: list)

  def apply(path: Path): Artifact = {
    apply(Resource.fromFile(path.path).reader.lines().toList, path)
  }

  def apply(lines: Seq[String], path: Path) = {
    val Attribute = """(.*) = (.*)""".r
    def toKV(line: String) = line match {
      case Attribute(k, v) => Some((k, v))
      case _ => None
    }
    val attributes = lines.flatMap(toKV).toMap
    new Artifact(attributes, path)
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

    files.map(Artifact.apply).filter(filter).toSeq
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
