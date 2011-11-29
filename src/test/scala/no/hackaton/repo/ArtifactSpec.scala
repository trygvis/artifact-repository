package no.hackaton.repo

import org.specs2.mutable._
import scala.collection.immutable.TreeMap
import scalax.file._

class ArtifactSpec extends Specification {
  import Artifact._

  val path = Path("awesome")
  "Artifact.apply()" should {
    "work" in {
      Artifact(path, List("a = x", "b = y", "c = z")).attributes must_== TreeMap.empty[String, String] ++ Seq(("a" -> "x"), ("b" -> "y"), ("c" -> "z"))
    }
  }

  "Artifact.artifactFilter" should {
    "work" in {
      val foo = Artifact(path, Seq("group-id = com.example", "artifact-id = foo"))
      val bar = Artifact(path, Seq("group-id = com.example", "artifact-id = bar"))
      val baz = Artifact(path, Seq("group-id = com.example", "artifact-id = baz"))

      val artifacts = List(foo, bar, baz)

      artifacts.filter(artifactFilter(Map("artifact-id" -> "baz"))) must_== List(baz)

      artifacts.filter(artifactFilter(Map("group-id" -> "com.example"))) must_== List(foo, bar, baz)

      artifacts.filter(artifactFilter(Map("group-id" -> "com.example", "artifact-id" -> "baz"))) must_== List(baz)
    }
  }

  "Artifact.urn" should {
    "work" in {
      Artifact(path, Seq("group-id = com.example", "artifact-id = baz")).urn must_== "urn:artifact:artifact-id=baz,group-id=com.example"
    }
  }
}
