case class Document(entries: List[Entry]) {
  def toXml = {
    <atom>
    </atom>
  }
}

case class Entry() {
}
