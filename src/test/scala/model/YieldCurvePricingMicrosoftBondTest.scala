package model

import finalibre.valuation.Valuation
import finalibre.valuation.model.YieldCurve
import finalibre.valuation.model.YieldCurve.Interpolation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.time.LocalDate

class YieldCurvePricingMicrosoftBondTest extends AnyFlatSpec with should.Matchers:

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

  val yc @ YieldCurve(_, yields, _) = YieldCurve.from(baseDate, knownYields, Interpolation.SplineInterpolator)
  val yieldMap = yields.map(p => p._1 -> (p._2 / 100.0 + 1.0)).toMap

  val isin = "US594918BY93"
  val currentPrice = 111.21

  val nominal = 100.0
  val couponRate = 3.3
  val maturityDate = LocalDate.of(2027, 2, 6)
  val nextCouponDate = LocalDate.of(2022,2,6)
  val annualCoupons = 2
  val couponDates = Stream.from(0).takeWhile(i => nextCouponDate.plusMonths(6 * i).isBefore(maturityDate)).map(i => nextCouponDate.plusMonths(6 * i)) :+ maturityDate
  val couponPaymentsForwardValue = couponDates.map(dat => (dat, nominal * ((couponRate / 100 )/2)))
  val couponPaymentsPresentValue = couponPaymentsForwardValue.map(p => (p._1, p._2, p._2 / yieldMap(p._1)))
  val nominalRedemptionPresentValue = nominal / yieldMap(maturityDate)
  val calculatedPrice = nominalRedemptionPresentValue + couponPaymentsPresentValue.map(_._3).sum

  given Valuation.BondCalculationContext = Valuation.bondCalculationContextFrom(baseDate, knownYields)
  val valuationCalculatedPrice = Valuation.calculateFixedCouponBulletBondPrice(nominal, couponRate, annualCoupons, nextCouponDate, maturityDate)

  "Valuation model" should "calculate this price for Microsoft Corporation bond" in
    (valuationCalculatedPrice should be (calculatedPrice +- 0.000000001))

  "Price of Microsoft Corporation bond, maturity: 06-02-2027 with 2 annual coupon payments at 3.3%" should "be valued close to currently traded price" in
    (valuationCalculatedPrice should be (currentPrice +- 6.1))

