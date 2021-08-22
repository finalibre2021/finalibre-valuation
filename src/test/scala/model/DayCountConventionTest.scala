package model

import finalibre.valuation.model.DayCountConvention.{Actual360, Actual365A, Actual365Fixed, Actual365L, ActualActualISDA, M30EY360, M30EY360ISDA, M30Y360US, NL365}
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.*
import flatspec.*
import matchers.*

import java.time.LocalDate
import java.time.temporal.ChronoUnit

class DayCountConventionTest extends AnyFlatSpec with should.Matchers:

  val inYearNoLeapStartDate = LocalDate.of(2021,2,2)
  val inYearNoLeapCurrentDate = LocalDate.of(2021,11,11)
  val inYearNoLeapEndDate = LocalDate.of(2022,2,2)

  val crossYearWithLeapStartDate = LocalDate.of(2019, 11, 11 )
  val crossYearWithLeapCurrentDate = LocalDate.of(2020, 3, 3 )
  val crossYearWithLeapEndDate = LocalDate.of(2020, 11, 11 )

  val deltaQuantsEx1StartDate = LocalDate.of(2007, 12, 28 )
  val deltaQuantsEx1CurrentDate = LocalDate.of(2008, 2, 28 )
  val deltaQuantsEx1EndDate = LocalDate.of(2008, 12, 28 )



  // 30U/360
  "30U/360" should "return 279 as number of days in non-leap year case" in
    (M30Y360US.countDays(inYearNoLeapStartDate, inYearNoLeapCurrentDate) should be (279))

  "30U/360" should "return 0.775 as day-fraction" in
    (M30Y360US.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.775))

  "30U/360" should "return 112 as number of days in leap year case" in
    (M30Y360US.countDays(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate) should be (112))

  "30U/360" should "return 0.311111 as day-fraction" in
    (M30Y360US.accruedFactor(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate, crossYearWithLeapEndDate, 1) should be (0.311111 +- 0.00001))

  "30U/360" should "return 60 as number of days in delta quants case 1" in
    (M30Y360US.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (60))

  "30U/360" should "return 0.166666666666667 as day-fraction" in
    (M30Y360US.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.166666666666667 +- 0.00001))




  // 30E/360
  "30E/360" should "return 279 as number of days in non-leap year case" in
    (M30EY360.countDays(inYearNoLeapStartDate, inYearNoLeapCurrentDate) should be (279))

  "30E/360" should "return 0.775 as day-fraction" in
    (M30EY360.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.775))

  "30E/360" should "return 112 as number of days in leap year case" in
    (M30EY360.countDays(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate) should be (112))

  "30E/360" should "return 0.311111 as day-fraction" in
    (M30EY360.accruedFactor(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate, crossYearWithLeapEndDate, 1) should be (0.311111 +- 0.00001))



  // Actual/365 Fixed (Act/365F)
  "Actual/365 Fixed" should "return 282 as number of days in non-leap year case" in
    (Actual365Fixed.countDays(inYearNoLeapStartDate, inYearNoLeapCurrentDate) should be (282))

  "Actual/365 Fixed" should "return 0.772603 as day-fraction" in
    (Actual365Fixed.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.772603 +- 0.00001))

  "Actual/365 Fixed" should "return 113 as number of days in leap year case" in
    (Actual365Fixed.countDays(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate) should be (113))

  "Actual/365 Fixed" should "return 0.309589 as day-fraction" in
    (Actual365Fixed.accruedFactor(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate, crossYearWithLeapEndDate, 1) should be (0.309589 +- 0.00001))

  "Actual/365 Fixed" should "return 62 as number of days in delta quants case 1" in
    (Actual365Fixed.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (62))

  "Actual/365 Fixed" should "return 0.16986301369863 as day-fraction in delta quants case 1" in
    (Actual365Fixed.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.16986301369863 +- 0.00001))


  // NL/365
  "NL/365" should "return 62 as number of days in delta quants case 1" in
    (NL365.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (62))

  "NL/365" should "return 0.16986301369863 as day-fraction in delta quants case 1" in
    (NL365.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.16986301369863 +- 0.00001))


  //Actual/Actual ISDA (Act/Act)
  ignore should "return 0.770492 as day-fraction" in
    (ActualActualISDA.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.770492 +- 0.00001))

  ignore should "return 0.308743 as day-fraction" in
    (ActualActualISDA.accruedFactor(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate, crossYearWithLeapEndDate, 1) should be (0.308743 +- 0.00001))

  "Actual/Actual ISDA" should "return 0.16942884946478 as day-fraction" in
    (ActualActualISDA.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.16942884946478 +- 0.00001))


  // 30/360 German (30E/360 ISDA)
  M30EY360ISDA


  // 30E+/360

  // Actual/365A
  "Actual/365A" should "return 62 as number of days in delta quants case 1" in
    (Actual365A.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (62))

  "Actual/365A" should "return 0.16986301369863 as day-fraction in delta quants case 1" in
    (Actual365A.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.16986301369863 +- 0.00001))


  // Actual/360 (Act/360)
  "Actual/360" should "return 62 as number of days in delta quants case 1" in
    (Actual360.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (62))

  "Actual/360" should "return 0.172222222222222 as day-fraction in delta quants case 1" in
    (Actual360.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.172222222222222 +- 0.00001))

  // Act/365L
  "Actual/365L" should "return 62 as number of days in delta quants case 1" in
    (Actual365L.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (62))

  "Actual/365L" should "return 0.169398907103825 as day-fraction in delta quants case 1" in
    (Actual365L.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.169398907103825 +- 0.00001))





