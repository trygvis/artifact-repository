package no.hackaton.repo.atom

import java.util.{Date, Calendar, TimeZone}
import scala.xml._

private object AtomUtils {
  def dateToAtomDate(date: Date) = {
    val sb = new StringBuilder()
    val c = Calendar.getInstance()
    c.setTimeZone(TimeZone.getTimeZone("UTC"))
    c.setTime(date)
    sb.append(c.get(Calendar.YEAR))
    sb.append('-')
    var f = c.get(Calendar.MONTH)
    if (f < 9)
        sb.append('0')
    sb.append(f + 1)
    sb.append('-')
    f = c.get(Calendar.DATE)
    if (f < 10)
        sb.append('0')
    sb.append(f)
    sb.append('T')
    f = c.get(Calendar.HOUR_OF_DAY)
    if (f < 10)
        sb.append('0')
    sb.append(f)
    sb.append(':')
    f = c.get(Calendar.MINUTE)
    if (f < 10)
        sb.append('0')
    sb.append(f)
    sb.append(':')
    f = c.get(Calendar.SECOND)
    if (f < 10)
        sb.append('0')
    sb.append(f)
    sb.append('.')
    f = c.get(Calendar.MILLISECOND)
    if (f < 100)
        sb.append('0')
    if (f < 10)
        sb.append('0')
    sb.append(f)
    sb.append('Z')
    sb.toString()
  }
}

import AtomUtils._

case class Person(name: String, uri: Option[String] = None, email: Option[String] = None) {
  def toXml =
    <author>
      <name>{name}</name>{uri.map(x => <uri>{x}</uri>).getOrElse(NodeSeq.Empty)}{email.map(x => <email>{x}</email>).getOrElse(NodeSeq.Empty)}
    </author>
}

case class Feed(id: String, title: String, updated: Date, author: Seq[Person] = Seq.empty, entries: Seq[Entry] = Seq.empty) {
  def toXml = {
    <feed xmlns="http://www.w3.org/2005/Atom">
      <title>{title}</title>
      <id>{id}</id>
      {author.map(_.toXml)}
      <updated>{dateToAtomDate(updated)}</updated>
      {entries.map(_.toXml)}
    </feed>
  }
}

case class Entry(title: String, id: String, updated: Date, links: Seq[Link] = Seq.empty, summary: Option[String] = None, author: Seq[Person] = Seq.empty) {
  def toXml = {
  <entry>
    <title>{title}</title>
    {links.map(_.toXml)}
    <id>{id}</id>
    <updated>{dateToAtomDate(updated)}</updated>
    {summary.map(s => <summary>{s}</summary>).getOrElse(NodeSeq.Empty)}
  </entry>
  }
}

case class Link(href: String, `type`: Option[String] = None, rel: Option[String] = None) {
  def toXml = {
    val attributes: MetaData = new UnprefixedAttribute("href", href, Null)
    val as = List(`type`.map(new UnprefixedAttribute("type", _, Null)), rel.map(new UnprefixedAttribute("rel", _, Null))).flatten
    val c = (attributes /: as)((xs: MetaData, a: MetaData) => xs.append(a))
    new Elem(null, "link", c, TopScope)
  }
}
