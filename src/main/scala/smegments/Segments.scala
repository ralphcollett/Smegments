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

case class BoundCoords(latitude: Double, longitude: Double)

case class Segment(id: Int, resourceState: ResourceState, name: String, climbCategory: Int, climbCategoryDesc: String, averageGrade: Double, startLatLong: BoundCoords, endLatLon: BoundCoords, elev_difference: Double, distance: Double, points: String, starred: Boolean)

case class Segments(segments: Set[Segment]) {
  def merge(other: Segments): Segments = {
    Segments(segments | other.segments)
  }
}

object Segments {

  def findMore(southWest: BoundCoords, northEast: BoundCoords, accessTokenString: String, depth: Int = 1): Either[Error, Segments] = {
    if (depth > 3) return Right(Segments(Set.empty))
    val longitudeMiddle = (northEast.longitude + southWest.longitude) / 2
    val latitudeMiddle = (southWest.latitude + northEast.latitude) / 2

    val segments = find(southWest, northEast, accessTokenString)
    List(
      (southWest, BoundCoords(latitudeMiddle, longitudeMiddle)),
      (BoundCoords(southWest.latitude, longitudeMiddle), BoundCoords(southWest.latitude, northEast.longitude)),
      (BoundCoords(latitudeMiddle, southWest.longitude), BoundCoords(northEast.longitude, longitudeMiddle)),
      (BoundCoords(latitudeMiddle, longitudeMiddle), northEast)
    ).map(coords => findMore(coords._1, coords._2, accessTokenString, depth + 1))
      .foldLeft(segments)((accumulatedSegments, searchResults) => accumulatedSegments.flatMap(s => searchResults.map(_.merge(s))))
  }

  def find(southWest: BoundCoords, northEast: BoundCoords, accessTokenString: String): Either[Error, Segments] = {
    val credentials = oAuth(accessToken(accessTokenString), new URL("https://www.strava.com/api"))

    val client = anApacheClient().`with`(credentials).`with`(httpTimeout(minutes(1)))
    val url = new URIBuilder("https://www.strava.com/api/v3/segments/explore")
      .addParameter("bounds", s"${southWest.latitude}," +
        s"${southWest.longitude}," +
        s"${northEast.latitude}," +
        s"${northEast.longitude}")
      .addParameter("activity_type", "running")
      .build().toURL

    implicit val decodeFoo: Decoder[Segment] = (c: HCursor) => for {
      id <- c.downField("id").as[Int]
      resourceStateId <- c.downField("resource_state").as[Int]
      name <- c.downField("name").as[String]
      climbCategory <- c.downField("climb_category").as[Int]
      climbCategoryDesc <- c.downField("climb_category_desc").as[String]
      startLatLong <- c.downField("start_latlng").as[Array[Double]]
      endLatLong <- c.downField("end_latlng").as[Array[Double]]
      averageGrade <- c.downField("avg_grade").as[Double]
      elevDifference <- c.downField("elev_difference").as[Double]
      distance <- c.downField("distance").as[Double]
      points <- c.downField("points").as[String]
      starred <- c.downField("starred").as[Boolean]
    } yield Segment(id, ResourceState(resourceStateId), name, climbCategory, climbCategoryDesc, averageGrade,
      BoundCoords(startLatLong(0), startLatLong(1)), BoundCoords(endLatLong(0), endLatLong(1)), elevDifference,
      distance, points, starred)

    val response = client.get(url, headers())

    if (response.ok()) {
      decode[Segments](response.getContent.asString()).left.map(error => JsonParseError(error.getMessage))
    } else {
      Left(HttpError(response.getStatusCode, response.getStatusMessage))
    }
  }
}
