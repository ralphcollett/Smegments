package smegments

sealed trait Error {
  def message: String
}
case class HttpError(httpCode: Int, httpResponse: String) extends Error {
  override def message: String = s"$httpCode - $httpResponse"
}
case class JsonParseError(message: String) extends Error