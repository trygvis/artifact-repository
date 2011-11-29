package no.hackaton.repo.atom

import no.hackaton.repo._
import org.specs2.mutable._
import scala.xml._

class AtomSpec extends Specification {
  import Artifact._

  "Link" should {
    "Link('my-href')" in { Link("my-href").toXml must_== <link href="my-href"/> }

    "Link('my-href', type=Some('type'))" in { Link("my-href", `type` = Some("type")).toXml must_== <link href="my-href" type="type"/> }

    "Link('my-href', rel=Some('rel'))" in { Link("my-href", rel = Some("rel")).toXml must_== <link href="my-href" rel="rel"/> }
  }
}

