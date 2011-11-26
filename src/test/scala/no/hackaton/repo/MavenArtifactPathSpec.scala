package no.hackaton.repo

import org.specs2.mutable._
import unfiltered.request._

class MavenArtifactPathSpec extends Specification {
  implicit def s2req(boo: String) = new HttpRequest[Unit] {
    def remoteAddr = sys.error("boo")
    def isSecure = sys.error("boo")
    def cookies = sys.error("boo")
    def headers(name: String) = sys.error("boo")
    def parameterValues(param: String) = sys.error("boo")
    def parameterNames = sys.error("boo")
    def uri = boo
    def method = sys.error("boo")
    def protocol = sys.error("boo")
    def reader = sys.error("boo")
    def inputStream = sys.error("boo")
  }

  "MavenArtifactPathSpec" should {
    "1" in {
      MavenArtifactPath.unapply("/boo") must beNone
    }

    "2" in {
      MavenArtifactPath.unapply("/maven-repo/com/example/foo/1.0-SNAPSHOT/foPSHOT-bin.jar") must beNone
    }

    "3" in {
      MavenArtifactPath.unapply("/maven-repo/com/example/foo/1.0-SNAPSHOT/foo-1.0-SNAPSHOT.jar") must beSome(("com.example", "foo", "1.0-SNAPSHOT", None, "jar"))
    }

    "4" in {
      MavenArtifactPath.unapply("/maven-repo/com/example/foo/1.0-SNAPSHOT/foo-1.0-SNAPSHOT-bin.jar") must beSome(("com.example", "foo", "1.0-SNAPSHOT", Some("bin"), "jar"))
    }

    "5" in {
      MavenArtifactPath.unapply("/maven-repo/com/example/test-project-sbt_2.9.1/1.0-SNAPSHOT/test-project-sbt_2.9.1-1.0-SNAPSHOT.pom") must beSome(("com.example", "test-project-sbt_2.9.1", "1.0-SNAPSHOT", None, "pom"))
    }
  }
}
