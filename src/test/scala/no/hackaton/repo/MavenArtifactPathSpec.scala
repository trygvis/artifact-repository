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
    val f = MavenArtifactPath.unapply _

    "1" in {
      f("/boo") must beNone
    }

    "2" in {
      f("/maven-repo/com/example/foo/1.0-SNAPSHOT/foPSHOT-bin.jar") must beNone
    }

    "3" in {
      f("/maven-repo/com/example/foo/1.0-SNAPSHOT/foo-1.0-SNAPSHOT.jar") must beSome(("com.example", "foo", "1.0-SNAPSHOT", None, "jar", None, None))
    }

    "4" in {
      f("/maven-repo/com/example/foo/1.0-SNAPSHOT/foo-1.0-SNAPSHOT-bin.jar") must beSome(("com.example", "foo", "1.0-SNAPSHOT", Some("bin"), "jar", None, None))
    }

    "5" in {
      f("/maven-repo/com/example/foo_2.9.1/1.0-SNAPSHOT/foo_2.9.1-1.0-SNAPSHOT.pom") must beSome(("com.example", "foo_2.9.1", "1.0-SNAPSHOT", None, "pom", None, None))
    }

    "6" in {
      f("/maven-repo/com/example/foo_2.9.1/1.0-SNAPSHOT/foo_2.9.1-1.0-SNAPSHOT.pom.sha1") must beSome(("com.example", "foo_2.9.1", "1.0-SNAPSHOT", None, "pom.sha1", None, None))
    }

    "7" in {
      f("/maven-repo/com/example/foo_2.9.1/1.0-SNAPSHOT/foo_2.9.1-1.0-SNAPSHOT-sources.jar.sha1") must beSome(("com.example", "foo_2.9.1", "1.0-SNAPSHOT", Some("sources"), "jar.sha1", None, None))
    }

    // Timestamp
    "8" in {
      f("/maven-repo/com/example/foo/1.0-SNAPSHOT/foo-1.0-20111126.124501-1.jar") must beSome(("com.example", "foo", "1.0-SNAPSHOT", None, "jar", Some("20111126.124501"), Some("1")))
    }

    // Timestamp + sha1
    "9" in {
      f("/maven-repo/com/example/foo/1.0-SNAPSHOT/foo-1.0-20111126.124501-1.jar.sha1") must beSome(("com.example", "foo", "1.0-SNAPSHOT", None, "jar.sha1", Some("20111126.124501"), Some("1")))
    }

    // classifier + timestamp + sha1
    "10" in {
      f("/maven-repo/com/example/foo/1.0-SNAPSHOT/foo-1.0-20111126.124501-1-sources.jar.sha1") must beSome(("com.example", "foo", "1.0-SNAPSHOT", Some("sources"), "jar.sha1", Some("20111126.124501"), Some("1")))
    }
  }
}
