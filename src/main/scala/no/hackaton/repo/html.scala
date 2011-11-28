package no.hackaton.repo

import scala.xml._

object HtmlTemplates {
  def main(body: NodeSeq) = 
  <html>
    <body>
      {body}
    </body>
  </html>

  def frontpage(urls: Urls) =
  <xml:group>
    <h1>Welcome to the Artifact Repository</h1>
    <h2>Forms</h2>
    <h3>Download Artifact</h3>
    <form action={urls.downloadArtifact()}>
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
        <td></td>
        <td><input type="submit"/></td>
      </tr>
      </table>
    </form>
  </xml:group>
}
