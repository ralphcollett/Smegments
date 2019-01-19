package smegments

import java.net.URL

import com.google.code.tempusfugit.temporal.Duration.minutes
import io.circe.{Decoder, HCursor}
import io.circe.generic.auto._
import io.circe.parser._
import org.apache.http.client.utils.URIBuilder
import simplehttp.HeaderList.headers
import simplehttp.HttpClients.anApacheClient
import simplehttp.configuration.AccessToken.accessToken
import simplehttp.configuration.HttpTimeout.httpTimeout
import simplehttp.configuration.OAuthCredentials.oAuth

case class BoundCoords(lat: Double, lon: Double)

case class Segment(id: Int, name: String, resourceState: ResourceState, averageGrade: Double, distance: Double)

case class Segments(segments: List[Segment])

object Segments {

  def find(southWest: BoundCoords, northEast: BoundCoords, accessTokenString: String): Either[Error, Segments] = {
    val credentials = oAuth(accessToken(accessTokenString), new URL("https://www.strava.com/api"))

    val client = anApacheClient().`with`(credentials).`with`(httpTimeout(minutes(1)))
    val url = new URIBuilder("https://www.strava.com/api/v3/segments/explore")
      .addParameter("bounds", s"${southWest.lat}," +
        s"${southWest.lon}," +
        s"${northEast.lat}," +
        s"${northEast.lon}")
      .addParameter("activity_type", "running")
      .build().toURL

    implicit val decodeFoo: Decoder[Segment] = (c: HCursor) => for {
      id <- c.downField("id").as[Int]
      name <- c.downField("name").as[String]
      resourceStateId <- c.downField("resource_state").as[Int]
      averageGrade <- c.downField("avg_grade").as[Double]
      distance <- c.downField("distance").as[Double]
    } yield Segment(id, name, ResourceState(resourceStateId), averageGrade, distance)

    val response = client.get(url, headers())

    if (response.ok()) {
      decode[Segments](response.getContent.asString()).left.map(error => JsonParseError(error.getMessage))
    } else {
      Left(HttpError(response.getStatusCode, response.getStatusMessage))
    }
  }
}
