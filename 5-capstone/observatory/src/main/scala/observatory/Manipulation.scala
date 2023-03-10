package observatory

import observatory.Visualization.predictTemperature

import scala.collection.parallel.CollectionConverters.given

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface:

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature =
    gridLoc => predictTemperature(temperatures, Location(gridLoc.lat, gridLoc.lon))

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature =
    gridLoc =>
      val temps: Iterable[Temperature] =
        temperaturess
          .map(makeGrid)
          .map(f => f(gridLoc))
      temps.sum / temps.size

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature =
    gridLoc => makeGrid(temperatures)(gridLoc) - normals(gridLoc)



