package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import com.sksamuel.scrimage.implicits.given

import scala.collection.parallel.CollectionConverters.given
import scala.collection.parallel.ParSeq
import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface:

  private val earthRadius: Double = 6371.0
  private val radians: Double = Math.PI / 180.0
  private val p: Int = 2
  private def isAntipodes(loc1: Location, loc2: Location): Boolean =
    loc1.lat == -loc2.lat && abs(loc1.lon - loc2.lon) == 180.0

  private def distance(loc1: Location, loc2: Location): Double = // TODO
    if loc1 == loc2 then
      0.0
    else if isAntipodes(loc1, loc2) then
      earthRadius * Math.PI
    else
      val deltaLon: Double = abs(loc1.lon - loc2.lon)
      earthRadius * acos(
        sin(loc1.lat * radians) * sin(loc2.lat * radians) +
        cos(loc1.lat * radians) * cos(loc2.lat * radians) * cos(deltaLon * radians))

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature =
    if temperatures.exists(_._1 == location) then // location=one of given locs
      temperatures.find(_._1 == location).get._2
    else
      val distances: Iterable[(Double, Temperature)] =
        temperatures map {
          case (loc, temp) => (distance(location, loc), temp)
        }

      distances.find(_._1 < 1.0) match
        case Some((_, temp)) => temp
        case None =>
          val weights: Iterable[Double] =
            distances.map(_._1)
              .map(pow(_, -p))

          val numerator: Double =
            weights.zip(temperatures.map(_._2))
              .map {
                case (w, t) => w * t
              }.sum

          val denominator: Double =
            weights.sum

          numerator / denominator

  private def interpolateYOnLineSegment(x0: Double, y0: Double,
                                        x1: Double, y1: Double, x: Double): Double =
    y0 + (x - x0) * (y1 - y0) / (x1 - x0)

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color =
    val sortedPoints: List[(Temperature, Color)] = points.toList.sortBy(_._1)

    if sortedPoints.exists(_._1 == value) then
      sortedPoints.find(_._1 == value).get._2
    else if sortedPoints.last._1 < value then
      sortedPoints.last._2
    else if value < sortedPoints.head._1 then
      sortedPoints.head._2
    else
      val (less, more) = sortedPoints.partition(_._1 < value)
      val (lowerBound, upperBound) = (less.last, more.head)

      val red: Double =
        interpolateYOnLineSegment(
          lowerBound._1, lowerBound._2.red,
          upperBound._1, upperBound._2.red, value)
      val green: Double =
        interpolateYOnLineSegment(
          lowerBound._1, lowerBound._2.green,
          upperBound._1, upperBound._2.green, value)
      val blue: Double =
        interpolateYOnLineSegment(
          lowerBound._1, lowerBound._2.blue,
          upperBound._1, upperBound._2.blue, value)

      Color(red.round.toInt, green.round.toInt, blue.round.toInt)

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): ImmutableImage =
    val (width, height) = (360, 180)
    val alpha = 255

    val pixels: IndexedSeq[(Int, Int)] =
      for
        x <- 0 until height
        y <- 0 until width
      yield (x, y)

    val locations: ParSeq[Location] =
      pixels.par.map {
        case (x, y) => Location(90 - x, y - 180)
      }

    val locationToTemperature: ParSeq[(Location, Temperature)] =
      locations.map(loc =>
        (loc, predictTemperature(temperatures, loc)))

    val coloredLocations: ParSeq[(Int, Int, Color)] =
      locationToTemperature.map {
        case (loc, temp) =>
          val (x, y) = (90 - loc.lat.toInt, loc.lon.toInt + 180)
          (x, y, interpolateColor(colors, temp))
      }

    val pixelArray: Array[Pixel] =
      coloredLocations.map {
        case (x, y, color) =>
          Pixel(x, y, color.red, color.green, color.blue, alpha)
        }.toArray

    ImmutableImage.wrapPixels(width, height, pixelArray, ImageMetadata.empty)


