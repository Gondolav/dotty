object Example {
  import scala.util.matching.Regex

  case class URL(protocol: Option[String], hostname: String, domain: String, path: Option[String])

  val regexURL: Regex = "(https?://)?(www.)?([a-z][a-z]*)(.)([a-z][a-z][a-z]*)(/[a-z]*)*".r
  val url: Option[URL] = "https://www.epfl.ch/schools/ic/" match {
    case regexURL(protocol, _, hostname, _, domain, path) => Some(URL(Option(protocol), hostname, domain, Option(path))) // here if an optional is not present, its value is null
    case _ => None
  }

  val protocolName: String = extractProtocolName(url)

  def extractProtocolName(url: Option[URL]): String = url match {
    case Some(URL(prot, _, _, _)) => prot match {
      case Some(s) => s.dropRight(3)
      case _ => "No protocol"
    }
    case _ => "No URL"
  }

  //---------------------------------------------------------------------------------

  val regexCompilationResult: Regex = "(Total time: )([0-9][0-9]*)( s, completed )([A-Z][a-z]*)( )([0-9][0-9]?)(, )([0-9][0-9][0-9][0-9])( )([0-9][0-9]?)(:)([0-9][0-9])(:)([0-9][0-9])( )([A-Z][A-Z])".r
  val compilationTime: Int = "Total time: 228 s, completed May 9, 2019 7:33:37 PM" match {
    case regexCompilationResult(_, time, x: _*) => time.toInt // if we call toInt on the wrong group, we get an exception at run-time
    case _ => -1
  }
}