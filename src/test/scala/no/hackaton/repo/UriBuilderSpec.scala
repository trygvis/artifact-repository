package no.hackaton.repo

import org.specs2.mutable._

class UriBuilderSpec extends Specification {
  def u(s: String) = new java.net.URL(s)

  "UriBuilder" should {
    "work" in {
      UrlBuilder("http://foo/a") / "b" must_== UrlBuilder("http://foo/a/b")

      UrlBuilder("http://foo/a") / "b" / "c" must_== UrlBuilder("http://foo/a/b/c")

      UrlBuilder("http://foo/a") ? ("x" -> "y")  must_== UrlBuilder("http://foo/a?x=y")

      (UrlBuilder("http://foo/a") / "b") ? ("x" -> "y")  must_== UrlBuilder("http://foo/a/b?x=y")
    }
  }
}
