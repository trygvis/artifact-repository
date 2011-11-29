package no.hackaton.repo

import scala.xml._

object HtmlTemplates {
  import ResponseUtils._

  def main(body: NodeSeq) = 
  <html>
    <body>
      {body}
    </body>
  </html>

  def searchResult(urls: Urls, params: Map[String, String], artifacts: ArtifactSeq) =
  <xml:group>
    <h1>Search Result</h1>
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

  def frontpage(urls: Urls) =
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
        <td></td>
        <td><input type="submit"/></td>
      </tr>
      </table>
    </form>
  </xml:group>
}
