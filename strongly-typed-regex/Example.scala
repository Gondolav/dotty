object Example {
  import scala.util.matching.Regex

  val regexURL: Regex = "(http[s-s]?://)?(www.)?([a-z][a-z]*)(.)([a-z][a-z][a-z]*)(/[a-z]*)*".r
  val informations: Option[List[String]] = "https://www.epfl.ch" match {
    case regexURL(protocol, _, hostname, _, domain, path) => Some(List(protocol, hostname, domain, path)) // here if an optional is not present, its value is null
    case _ => None
  }

  val protocolName: String = informations.get.head.dropRight(3) // if protocol is not present, runtime error


  // TODO
  val regexCompilationResult: Regex = "".r
  val compilationTime: Int = "Total time: 228 s, completed May 9, 2019 7:33:37 PM" match {
    case regexCompilationResult() => compileTime.toInt
  }
}