package no.hackaton

package object repo {
  /* This is what I really want it to be. The order is significat (order fucks up equivalence between "same" URIs), and whether it has an argument or not.
   *
   * /foo/bar?a=1&b=2 != /foo/bar/?b=2&a=1
   *
   * /foo/bar?a= != /foo/bar/a
   */
//  type QueryParameters = Seq[(String,Option[String])]

  // This is what everyone uses, boo
  type QueryParameters = Map[String,Seq[String]]

  // TODO: s,Seq,Stream,
  type ArtifactSeq = Seq[Artifact]

  type Attributes = Map[String, String]

  def utf8Charset = java.nio.charset.Charset.forName("utf-8")
}
