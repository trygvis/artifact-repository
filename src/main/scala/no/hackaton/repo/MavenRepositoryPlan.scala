package no.hackaton.repo

import unfiltered.filter._
import unfiltered.request._
import unfiltered.response._

class MavenRepositoryPlan extends Plan {
  def intent = {
    case MavenArtifactPath(groupId, artifactId, version, classifier, t) =>
//      println("MavenRepositoryPlan: type= " + t)
      if(t.endsWith(".sha1") || t.endsWith(".md5")) {
        Redirect("/dev/null")
      }
      else {
        Redirect("/upload?group-id=" + groupId + "&artifact-id=" + artifactId + "&version=" + version + classifier.map("&classifier=" + _).getOrElse("") + "&type=" + t)
      }
  }
}

object MavenArtifactPath {
//  def paths = """^/maven-repo\(/[a-zA-Z0-9]*\)""".r

//  def p2 = """^\([a-zA-Z0-9]*\)-[a-zA-Z0-9]*-\([a-zA-Z0-9]*\)\.\([a-zA-Z0-9]*\)""".r

  def unapply[T](req: HttpRequest[T]): Option[(String, String, String, Option[String], String)] = {
    import java.util.regex._
    import java.util.regex.Pattern.quote

//    println("req.uri=" + req.uri)
    if(!req.uri.startsWith("/maven-repo/"))
      return None

    val path = req.uri.substring(11)
//    val pattern = "\\(/[a-zA-Z0-9]*\\)".r
    val pattern = "/[-._a-zA-Z0-9]*".r
    val typeR = "\\.[a-zA-Z0-9]*$".r
    val segments = pattern.findAllIn(path).map(_.substring(1)).toList.reverse
//    println("path=" + path)
//    println("segments=" + segments)
    segments match {
      case List(file, version, artifactId, groupId @ _*) => 
//        println("---------------")
//        println(("\\(" + quote(artifactId + "-" + version) + "\\)-\\(.*\\)\\.\\([a-zA-Z0-9]\\)*").r.findAllIn(file).toList)
//        var r = ("\\(" + quote(artifactId + "-" + version) + "\\)\\.\\([a-zA-Z0-9]*\\)")
        var r = ("(" + quote(artifactId + "-" + version) + ")((-)([_a-zA-Z0-9]*))?\\.([.a-zA-Z0-9]*)$")
//        var r = ("(" + quote(artifactId) + ")-((2.0-SNAPSHOT)|([.-a-zA-Z0-9]*))?\\.([a-zA-Z0-9]*)")
//        r = "(foo-1.0-SNAPSHOT).(jar)"
        /*
        println("r = " + r)
        println("r.pattern = " + r.r.pattern)
        println("file=" + file)
        println("matches=" + r.r.findAllIn(file))
        println("matches=" + r.r.findAllIn(file).matchData)
        println("subgroups=" + r.r.findAllIn(file).subgroups)
        println("---------------")
        */
        /*
        println("r = " + r)
        println("file = " + file)
        val matcher = Pattern.compile(r).matcher(file)
        val find = matcher.find()
        println("matcher.find=" + find)
        println("matcher.groupCount=" + matcher.groupCount())
        if(find) {
          (1 to matcher.groupCount).foreach{i => println("i=" + i + ", match=" + matcher.group(i)) }
        }
        println("---------------")
        */

        val matcher = Pattern.compile(r).matcher(file)
        if(matcher.find()) {
          Some((groupId.reverse.mkString("."), artifactId, version, Option(matcher.group(4)), matcher.group(5)))
        }
        else {
          None
        }

        /*
        typeR.findFirstIn(file) match {
          case Some(t) => 
//            val c = Some(file.substring(artifactId.length + version.length + 2).substring(0, t.length - 1)).filter(!_.isEmpty)
            println("f = " + file)
            val x = "\\(" + quote(artifactId + "-" + version) + "\\)-\\(.*\\)" + quote(t)
            println("x = " + x)
//            println("c = " + x.r.findFirstIn(file))
            xs = x.r.findAllIn(file).toList
            println("c = " + xs)
            xs match {
              case List(_, c, _) =>
                Some((groupId.reverse.mkString("."), artifactId, version, c, t.substring(1)))
            }
            val c = None
            Some((groupId.reverse.mkString("."), artifactId, version, c, t.substring(1)))
          case _ => None
        }
        */
      case _ => None
    }
  }
}
