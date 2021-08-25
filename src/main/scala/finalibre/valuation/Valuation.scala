package finalibre.valuation

import finalibre.valuation.Extensions._
import finalibre.valuation.model.YieldCurve
import finalibre.valuation.model.YieldCurve.Interpolation


import org.apache.commons.math3.analysis.interpolation.UnivariateInterpolator

import java.time.LocalDate

object Valuation {

  def calculateFixedCouponBulletBondPrice(nominalBase : Double, couponRate : Double, couponsPerYear : Int, firstCouponDate : LocalDate, maturityDate : LocalDate, spread : Double = 0.0)(using context : BondCalculationContext) : Double =
    val monthStep = 12 / couponsPerYear
    val allCouponDates = Stream.from(0).takeWhile(i => firstCouponDate.plusMonths(monthStep * i).isBefore(maturityDate)).map(i => firstCouponDate.plusMonths(i * monthStep)) :+ maturityDate
    val remainingCouponDates = allCouponDates.filter(dat => dat.isAfter(context.baseDate) || dat == context.baseDate)
    val couponPaymentsForwardValue = remainingCouponDates.map(dat => (dat, nominalBase * (couponRate / 100.0 )  / couponsPerYear.toDouble))
    val couponPaymentsPresentValue = couponPaymentsForwardValue.map(en => (en._1, context.discount(en._2, en._1, spread)))
    val redemptionPresentValue = context.discount(nominalBase, maturityDate, spread)
    val totalValue = redemptionPresentValue + couponPaymentsPresentValue.map(_._2).sumPossiblyEmpty
    totalValue


  def creditSpreadFor(nominalBase : Double, couponRate : Double, couponsPerYear : Int, firstCouponDate : LocalDate, maturityDate : LocalDate, marketPrice : Double)(using context : BondCalculationContext) : Double =
    val calcPrice : Double => Double = (spr : Double) => calculateFixedCouponBulletBondPrice(nominalBase, couponRate, couponsPerYear, firstCouponDate, maturityDate, spr)
    val priceNoSpread = calcPrice(0.0)
    if priceNoSpread < marketPrice then 0.0
    else
      val maxSpread = Stream.from(1).find(i => calcPrice(0.001 * i.toDouble) < marketPrice).get * 0.001
      maxSpread - 0.001



  def bondCalculationContextFrom(baseDate : LocalDate, knownYields : Seq[(LocalDate, Double)], yieldInterpolator : Interpolation.Interpolator = Interpolation.SplineInterpolator) = BondCalculationContext(baseDate, knownYields, yieldInterpolator)

  case class BondCalculationContext(baseDate : LocalDate, knownYields : Seq[(LocalDate, Double)], yieldInterpolator : Interpolation.Interpolator):
    lazy val yieldCurve : YieldCurve =
      YieldCurve.from(baseDate, knownYields, yieldInterpolator)
    lazy val yieldMap : Map[LocalDate, Double] = yieldCurve.yields.map(en => (en._1, en._2 / 100.0 + 1.0)).toMap
    def discount(value : Double, dat : LocalDate, spread : Double = 0.0) = value / (yieldMap(dat) + spread)



}
