package no.hackaton.repo

import unfiltered.filter._
import unfiltered.request._
import unfiltered.response._
import java.util.{Calendar, TimeZone}
import java.util.regex._
import java.util.regex.Pattern.quote

class MavenRepositoryPlan extends Plan {
  def utc = TimeZone.getTimeZone("UTC")
  def now() = {
    val cal = Calendar.getInstance(utc)
    "" + cal.get(Calendar.YEAR) + cal.get(Calendar.MONTH) + cal.get(Calendar.DAY_OF_MONTH) + "." + cal.get(Calendar.HOUR_OF_DAY) + cal.get(Calendar.MINUTE) + cal.get(Calendar.SECOND)
  }

  def intent = {
    case MavenArtifactPath(groupId, artifactId, version, classifier, t, timestamp, buildNumber) =>
//      println("MavenRepositoryPlan: type= " + t)
      if(t.endsWith(".sha1") || t.endsWith(".md5")) {
        Redirect("/dev/null")
      }
      else {
        Redirect("/upload?group-id=" + groupId + "&artifact-id=" + artifactId + "&version=" + version + classifier.map("&classifier=" + _).getOrElse("") + "&type=" + t + "&timestamp=" + timestamp.getOrElse(now) + buildNumber.map("&build-number=" + _).getOrElse(""))
      }
    case MavenMetadata(_) =>
      Redirect("/dev/null")
  }
}

object MavenMetadata {
  val regex = Pattern.compile("^/" + MavenArtifactPath.prefix + "/.*/maven-metadata.xml$")
  def unapply[T](req: HttpRequest[T]): Option[Any] = {
    Some(req.uri).filter(regex.matcher(_).find)
  }
}

object MavenArtifactPath {
  def prefix = "maven-repo"
//  def paths = """^/maven-repo\(/[a-zA-Z0-9]*\)""".r

//  def p2 = """^\([a-zA-Z0-9]*\)-[a-zA-Z0-9]*-\([a-zA-Z0-9]*\)\.\([a-zA-Z0-9]*\)""".r

  // (groupId, artifactId, version, classifier, type, timestamp, buildNumber)
  def unapply[T](req: HttpRequest[T]): Option[(String, String, String, Option[String], String, Option[String], Option[String])] = {
//    println("req.uri=" + req.uri)
    if(!req.uri.startsWith("/" + prefix + "/"))
      return None

    val path = req.uri.substring(11)
//    val pattern = "\\(/[a-zA-Z0-9]*\\)".r
    val pattern = "/[-._a-zA-Z0-9]*".r
    val typeR = "\\.[a-zA-Z0-9]*$".r
    val segments = pattern.findAllIn(path).map(_.substring(1)).toList.reverse
//    println("path=" + path)
//    println("segments=" + segments)
    segments match {
      case List(file, version, artifactId, groupId @ _*) if file.startsWith(artifactId + "-") => 

        val f = file.substring(artifactId.length + 1)
        val timestamp = "^(.*)-([0-9]{8})\\.([0-9]{6})\\-([0-9]*)(-([a-zA-Z0-9]*))?\\.(.*)$"
        val timestampM = Pattern.compile(timestamp).matcher(f)
        val snapshot = "^.*-SNAPSHOT(-([a-zA-Z0-9]*))?\\.(.*)$"
        val snapshotM = Pattern.compile(snapshot).matcher(f)

        // (groupId, artifactId, version, classifier, type, timestamp, buildNumber)
        if(timestampM.find()) {
          val t = for {
            date <- Option(timestampM.group(2))
            time <- Option(timestampM.group(3))
          } yield date + "." + time
          // TODO: Build a internet date out of the timestamp
          Some((groupId.reverse.mkString("."), artifactId, version, Option(timestampM.group(6)), timestampM.group(7), t, Option(timestampM.group(4))))
        }
        else if(snapshotM.find()) {
          Some((groupId.reverse.mkString("."), artifactId, version, Option(snapshotM.group(2)), snapshotM.group(3), None, None))
        }
        else {
          None
        }
      case _ => None
    }
  }
}
