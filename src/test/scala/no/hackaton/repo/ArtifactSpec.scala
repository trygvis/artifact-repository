package no.hackaton.repo

import org.specs2.mutable._
import scalax.file._

class ArtifactSpec extends Specification {
  import Artifact._

  val path = Path("awesome")
  "Artifact.apply()" should {
    "work" in {
      Artifact(List("a = x", "b = y", "c = z"), path).attributes must_== Map(("a" -> "x"), ("b" -> "y"), ("c" -> "z"))
    }
  }

  "Artifact.artifactFilter" should {
    "work" in {
      val foo = Artifact(Map("group-id" -> "com.example", "artifact-id" -> "foo"), Path("x"))
      val bar = Artifact(Map("group-id" -> "com.example", "artifact-id" -> "bar"), Path("x"))
      val baz = Artifact(Map("group-id" -> "com.example", "artifact-id" -> "baz"), Path("x"))

      val artifacts = List(foo, bar, baz)

      artifacts.filter(artifactFilter(Map("artifact-id" -> "baz"))) must_== List(baz)

      artifacts.filter(artifactFilter(Map("group-id" -> "com.example"))) must_== List(foo, bar, baz)

      artifacts.filter(artifactFilter(Map("group-id" -> "com.example", "artifact-id" -> "baz"))) must_== List(baz)
    }
  }
}
