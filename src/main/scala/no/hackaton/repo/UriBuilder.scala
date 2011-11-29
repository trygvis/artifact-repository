package no.hackaton.repo

import java.net.URL

case class UrlBuilder(protocol: String, host: String, port: Int, path: String, query: QueryParameters) {
  lazy val hostPort =
    if((protocol == "http" && port == 80) || (protocol.equals("https") && port == 443))
      host
    else
      host + ":" + port

  def asUrl: URL = new URL(protocol + "://" + hostPort + path + toString(query))
  def apply(): String = asUrl.toExternalForm

//  def toString(q: QueryParameters) = if(q.isEmpty) "" else "?" + q.foldLeft("")((s, p) => p match {
//    case (key, Some(value)) => s + "&" + key + "=" + value
//    case (key, None) => s + "&" + key
//  })

  def toString(q: QueryParameters) = if(q.isEmpty) "" else "?" + q.foldLeft("")((s, p) => p match {
    case (key, values) => s + values.foldLeft("")((s, value) => "&" + key + "=" + value)
  })

  def /(seg: String) = if(path.equals("/")) copy(path = "/" + seg) else copy(path = path + "/" + seg)

  def ?(key: String, value: String): UrlBuilder = {
    // map((values: Seq[String]) => value + values).
    val values: Seq[String] = query.get(key).getOrElse(Seq(value))
    copy(query = (query + ((key, values))))
  }

  def ?(kv: (String, String)): UrlBuilder = ?(kv._1, kv._2)

  def ?(map: Map[String, String]): UrlBuilder = copy(query = query ++ map.mapValues(Seq(_)))
}

object UrlBuilder {
  def apply(url: String): UrlBuilder = apply(new URL(url))

  def apply(url: URL) = {
    val p = url.getPort
    new UrlBuilder(url.getProtocol, url.getHost, if(p == -1) url.getDefaultPort else p, url.getPath, Option(url.getQuery).map(queryFromString).getOrElse(Map.empty))
  }

  def queryFromString(q: String) = q.split("&").map { x =>
    x.split("=") match {
      case Array(k) => (k, Seq.empty)
      case Array(k, v) => (k, Seq(v))
    }
  } toMap
}

