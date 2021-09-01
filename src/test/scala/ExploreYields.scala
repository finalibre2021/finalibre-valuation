import finalibre.valuation.model.YieldCurve
import finalibre.valuation.model.YieldCurve.Interpolation
import org.jfree.chart.{ChartFactory, ChartUtils}
import org.jfree.data.time.{Day, TimeSeries, TimeSeriesCollection}


import java.io.File
import java.time.LocalDate

object ExploreYields {

  def main(args: Array[String]): Unit =
    val currency = "USD"
    val baseDate = LocalDate.of(2021, 8, 20)
    val knownYields = List(
      baseDate.plusMonths(1) -> 0.04,
      baseDate.plusMonths(3) -> 0.05,
      baseDate.plusMonths(6) -> 0.05,
      baseDate.plusMonths(1) -> 0.06,
      baseDate.plusYears(1) -> 0.23,
      baseDate.plusYears(2) -> 0.44,
      baseDate.plusYears(3) -> 0.80,
      baseDate.plusYears(5) -> 1.06,
      baseDate.plusYears(7) -> 1.26,
      baseDate.plusYears(10) -> 1.79,
      baseDate.plusYears(20) -> 1.87)

    val yieldsBySpline = YieldCurve.from(baseDate, knownYields, Interpolation.SplineInterpolator)
    val yieldsByAkima = YieldCurve.from(baseDate, knownYields, Interpolation.AkimaInterpolator)
    val yieldsByLoess = YieldCurve.from(baseDate, knownYields, Interpolation.LoessInterpolator)

    val toChart = List(
      "Spline" -> yieldsBySpline,
      "Akima" -> yieldsByAkima,
      "Loess" -> yieldsByLoess
    )

    val dataSet = new TimeSeriesCollection()
    toChart.foreach {
      case (name, yc) =>
        val series = new TimeSeries(name)
        yc.yields.foreach(y => series.add(new Day(y._1.getDayOfMonth, y._1.getMonthValue, y._1.getYear), y._2))
        dataSet.addSeries(series)
    }
    val chart = ChartFactory.createTimeSeriesChart("Interpolaters", "Period", "Yield", dataSet)
    ChartUtils.saveChartAsPNG(new File(s"c:\\temp\\interpolaters_${System.currentTimeMillis()}.png"), chart, 1200, 1200)


}
