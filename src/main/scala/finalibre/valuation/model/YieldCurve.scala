package finalibre.valuation.model

import org.apache.commons.math3.analysis.interpolation as commonInterpol

import java.time.LocalDate
import java.time.temporal.ChronoUnit

case class YieldCurve(
                       baseDate : LocalDate,
                       yields : Seq[(LocalDate, Double)],
                       basedOn : Seq[(LocalDate, Double)]
                     )

object YieldCurve:

  object Interpolation:
    sealed trait Interpolator(val interpol : commonInterpol.UnivariateInterpolator)
    object SplineInterpolator extends Interpolator(new commonInterpol.SplineInterpolator)
    object AkimaInterpolator extends Interpolator(new commonInterpol.AkimaSplineInterpolator)
    object LoessInterpolator extends Interpolator(new commonInterpol.LoessInterpolator)

  def from(baseDate : LocalDate, knownYields : Seq[(LocalDate, Double)], interpolater : Interpolation.Interpolator) : YieldCurve =
    if knownYields.isEmpty then YieldCurve(baseDate, Nil, knownYields)
    else
      val epochDayGroups = knownYields.groupBy(en => en._1.toEpochDay)
      val (xs,ys) = epochDayGroups.toSeq.sortBy(en => en._1).map(en => daysBetween(baseDate, en._2.head._1).toDouble -> en._2.head._2).unzip
      val function = interpolater.interpol.interpolate(xs.toArray, ys.toArray)
      val (minDate,maxDate) = (knownYields.map(_._1).minBy(_.toEpochDay), knownYields.map(_._1).maxBy(_.toEpochDay))
      val (daysToMin, daysToMax) = (daysBetween(baseDate, minDate), daysBetween(baseDate, maxDate))
      val yields = for(numDays <- daysToMin to daysToMax) yield (baseDate.plusDays(numDays), function.value(numDays))
      YieldCurve(baseDate, yields, knownYields)

  def daysBetween(fromDate : LocalDate, date : LocalDate) : Int = ChronoUnit.DAYS.between(fromDate, date).toInt



