package no.hackaton.repo

import java.net.URL

case class UrlBuilder(protocol: String, host: String, port: Int, path: String, query: QueryParameters) {
  lazy val hostPort =
    if((protocol == "http" && port == 80) || (protocol.equals("https") && port == 443))
      host
    else
      host + ":" + port

  def toUrl: URL = {
    println("host = " + host)
    println("hostPort = " + hostPort)
    new URL(protocol + "://" + hostPort + path + toString(query))
  }
  def apply(): String = toUrl.toExternalForm

//  def toString(q: QueryParameters) = if(q.isEmpty) "" else "?" + q.foldLeft("")((s, p) => p match {
//    case (key, Some(value)) => s + "&" + key + "=" + value
//    case (key, None) => s + "&" + key
//  })

  def toString(q: QueryParameters) = if(q.isEmpty) "" else "?" + q.foldLeft("")((s, p) => p match {
    case (key, values) => s + values.foldLeft("")((s, value) => "&" + key + "=" + value)
  })

  def /(seg: String) = if(path.equals("/")) copy(path = "/" + seg) else copy(path = path + "/" + seg)
}

object UrlBuilder {
  def apply(url: String): UrlBuilder = apply(new URL(url))

  def apply(url: URL) = {
    val HostPort = """^(\S+[:])(\d{4})$""".r
    println("url.getHost=" + url.getHost)
    val (host, port) = url.getHost match {
      case HostPort(host, port) => (host, port.toInt)
      case _ => sys.error("boo")
    }
    new UrlBuilder(url.getProtocol, host, port, url.getPath, Option(url.getQuery).map(queryFromString).getOrElse(Map.empty))
  }

  def queryFromString(q: String) = sys.error("not implemented")
}

