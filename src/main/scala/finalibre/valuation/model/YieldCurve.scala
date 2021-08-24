package finalibre.valuation.model

import org.apache.commons.math3.analysis.interpolation.{SplineInterpolator, UnivariateInterpolator}

import java.time.LocalDate
import java.time.temporal.ChronoUnit

case class YieldCurve(
                       currency : String,
                       baseDate : LocalDate,
                       yields : Seq[(LocalDate, Double)],
                       basedOn : Seq[(LocalDate, Double)]
                     )

object YieldCurve:
  def from(currency : String, baseDate : LocalDate, knownYields : Seq[(LocalDate, Double)], interpolater : UnivariateInterpolator = new SplineInterpolator()) : YieldCurve =
    if knownYields.isEmpty then YieldCurve(currency, baseDate, Nil, knownYields)
    else
      val epochDayGroups = knownYields.groupBy(en => en._1.toEpochDay)
      val (xs,ys) = epochDayGroups.toSeq.sortBy(en => en._1).map(en => daysBetween(baseDate, en._2.head._1).toDouble -> en._2.head._2).unzip
      val function = interpolater.interpolate(xs.toArray, ys.toArray)
      val (minDate,maxDate) = (knownYields.map(_._1).minBy(_.toEpochDay), knownYields.map(_._1).maxBy(_.toEpochDay))
      val (daysToMin, daysToMax) = (daysBetween(baseDate, minDate), daysBetween(baseDate, maxDate))
      val yields = for(numDays <- daysToMin to daysToMax) yield (baseDate.plusDays(numDays), function.value(numDays))
      YieldCurve(currency, baseDate, yields, knownYields)

  def daysBetween(fromDate : LocalDate, date : LocalDate) : Int = ChronoUnit.DAYS.between(fromDate, date).toInt



