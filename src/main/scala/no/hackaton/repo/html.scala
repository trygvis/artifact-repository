package no.hackaton.repo

import scala.xml._

case class Link(href: String, rel: String, want: Want) {
  def toXml = <link href={href} rel={rel} type={want.mediaType}/>
}

object HtmlTemplates {
  implicit def url2string(x: UrlBuilder) = x()

  import ResponseUtils._

  def main(title: String, links: Seq[Link] = Seq.empty, body: NodeSeq) = 
  <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <meta charset="UTF-8"/>
      <title>{title}</title>
      {links.map(_.toXml)}
    </head>
    <body>{body}</body>
  </html>

  def header(urls: Urls, title: String) =
  <xml:group>
    <a href={urls.start}>Start</a>
    <h1>{title}</h1>
  </xml:group>

  def searchResult(implicit urls: Urls, params: Map[String, String], artifacts: ArtifactSeq) =
    main("Search Results", links = List(Link(urls.search(params, Some(WantAtom)), "self", WantAtom)), body = searchResultBody)

  def searchResultBody(implicit urls: Urls, params: Map[String, String], artifacts: ArtifactSeq) =
  <xml:group>
    {header(urls, "Search Result")}
    <h2>Input</h2>
    <ul>
      <li>Query: {queryToString(params)}</li>
      <li><a href={urls.search(params, mediaType = Some(WantAtom))()}>Search as atom</a></li>
    </ul>
    <h2>Hits</h2>
    {artifacts.map(toHtml)}
  </xml:group>

  def toHtml(a: Artifact) =
    <div>
      <ul>
        <li>Timestamp: {a.timestamp}</li>
        {a.attributes.map{kv => <li>{kv._1 + ": " + kv._2}</li>}}
      </ul>
    </div>

  def frontpage(implicit urls: Urls) = {
    main("Artifact Repository", body = frontpageBody)
  }

  def frontpageBody(implicit urls: Urls) = 
  <xml:group>
    <h1>Welcome to the Artifact Repository</h1>
    <h2>Forms</h2>
    <h3>Search for Artifacts</h3>
    <form action={urls.search()}>
      <table>
      <tr>
        <td>Group id</td>
        <td><input type="text" name="group-id"/></td>
      </tr>
      <tr>
        <td>Artifact id</td>
        <td><input type="text" name="artifact-id"/></td>
      </tr>
      <tr>
        <td>Version</td>
        <td><input type="text" name="version"/></td>
      </tr>
      <tr>
        <td>Type</td>
        <td><input type="text" name="type"/></td>
      </tr>
      <tr>
        <td>Classifier</td>
        <td><input type="text" name="classifier"/></td>
      </tr>
      <tr>
        <td>Direct download?</td>
        <td><input type="text" name="direct-download"/></td>
      </tr>
      <tr>
        <td>Content type</td>
        <td>
          <select name="media-type">
            <option value="html">HTML (text/html)</option>
            <option value="text">Plain text (text/plain)</option>
            <option value="atom">Atom (application/atom+xml)</option>
          </select>
        </td>
      </tr>
      <tr>
        <td><!-- empty --></td>
        <td><input type="submit"/></td>
      </tr>
      </table>
    </form>
  </xml:group>
}
