package model

import finalibre.valuation.model.DayCountConvention.{Actual365Fixed, ActualActualISDA, M30EY360, M30Y360US}
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.*
import flatspec.*
import matchers.*

import java.time.LocalDate

class DayCountConventionTest extends AnyFlatSpec with should.Matchers:

  val inYearNoLeapStartDate = LocalDate.of(2021,2,2)
  val inYearNoLeapCurrentDate = LocalDate.of(2021,11,11)
  val inYearNoLeapEndDate = LocalDate.of(2022,2,2)

  val crossYearWithLeapStartDate = LocalDate.of(2019, 11, 11 )
  val crossYearWithLeapCurrentDate = LocalDate.of(2020, 3, 3 )
  val crossYearWithLeapEndDate = LocalDate.of(2020, 11, 11 )


  // 30U/360
  "30U/360" should "return 279 as number of days in non-leap year case" in
    (M30Y360US.countDays(inYearNoLeapStartDate, inYearNoLeapCurrentDate) should be (279))

  "30U/360" should "return 0.775 as day-fraction" in
    (M30Y360US.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.775))

  "30U/360" should "return 112 as number of days in leap year case" in
    (M30Y360US.countDays(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate) should be (112))

  "30U/360" should "return 0.311111 as day-fraction" in
    (M30Y360US.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.311111))



  // 30E/360
  "30E/360" should "return 279 as number of days in non-leap year case" in
    (M30EY360.countDays(inYearNoLeapStartDate, inYearNoLeapCurrentDate) should be (279))

  "30E/360" should "return 0.775 as day-fraction" in
    (M30EY360.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.775))

  "30E/360" should "return 112 as number of days in leap year case" in
    (M30EY360.countDays(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate) should be (112))

  "30E/360" should "return 0.311111 as day-fraction" in
    (M30EY360.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.311111))



  // Actual/365 Fixed
  "Actual/365 Fixed" should "return 282 as number of days in non-leap year case" in
    (Actual365Fixed.countDays(inYearNoLeapStartDate, inYearNoLeapCurrentDate) should be (282))

  "Actual/365 Fixed" should "return 0.772603 as day-fraction" in
    (Actual365Fixed.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.772603 +- 0.00001))

  "Actual/365 Fixed" should "return 113 as number of days in leap year case" in
    (Actual365Fixed.countDays(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate) should be (112))

  "Actual/365 Fixed" should "return 0.309589 as day-fraction" in
    (Actual365Fixed.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.309589))


  //"Actual/Actual ISDA"
  ignore should "return 0.770492 as day-fraction" in
    (ActualActualISDA.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.770492 +- 0.00001))

  "Actual/Actual ISDA" should "return 0.308743 as day-fraction" in
    (ActualActualISDA.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.308743))
