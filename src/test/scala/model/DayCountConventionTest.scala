package model

import finalibre.valuation.model.DayCountConvention.{Actual360, Actual365A, Actual365Fixed, Actual365L, ActualActualISDA, M30EPlusY360, M30EY360, M30EY360ISDA, M30Y360German, M30Y360US, NL365}
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

  val deltaQuantsEx2StartDate = LocalDate.of(2007, 12, 28 )
  val deltaQuantsEx2CurrentDate = LocalDate.of(2008, 2, 29 )
  val deltaQuantsEx2EndDate = LocalDate.of(2008, 12, 28 )

  val deltaQuantsEx3StartDate = LocalDate.of(2007, 10, 31 )
  val deltaQuantsEx3CurrentDate = LocalDate.of(2008, 11, 30 )
  val deltaQuantsEx3EndDate = LocalDate.of(2008, 10, 31 )

  val deltaQuantsEx4StartDate = LocalDate.of(2008, 1, 2 )
  val deltaQuantsEx4CurrentDate = LocalDate.of(2009, 5, 31 )
  val deltaQuantsEx4EndDate = LocalDate.of(2009, 1, 2 )

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

  "30U/360" should "return 61 as number of days in delta quants case 1" in
    (M30Y360US.countDays(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate) should be (61))

  "30U/360" should "return 0.169444444444444 as day-fraction" in
    (M30Y360US.accruedFactor(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate, deltaQuantsEx2EndDate, 1) should be (0.169444444444444 +- 0.00001))

  "30U/360" should "return 390 as number of days in delta quants case 1" in
    (M30Y360US.countDays(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate) should be (390))

  "30U/360" should "return 1.108333333333333 as day-fraction" in
    (M30Y360US.accruedFactor(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate, deltaQuantsEx3EndDate, 1) should be (1.108333333333333 +- 0.00001))

  "30U/360" should "return 480 as number of days in delta quants case 1" in
    (M30Y360US.countDays(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate) should be (480))

  "30U/360" should "return 1.33333333333333 as day-fraction" in
    (M30Y360US.accruedFactor(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate, deltaQuantsEx4EndDate, 1) should be (1.33333333333333 +- 0.00001))

  // 30E/360
  "30E/360" should "return 279 as number of days in non-leap year case" in
    (M30EY360.countDays(inYearNoLeapStartDate, inYearNoLeapCurrentDate) should be (279))

  "30E/360" should "return 0.775 as day-fraction" in
    (M30EY360.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.775))

  "30E/360" should "return 112 as number of days in leap year case" in
    (M30EY360.countDays(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate) should be (112))

  "30E/360" should "return 0.311111 as day-fraction" in
    (M30EY360.accruedFactor(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate, crossYearWithLeapEndDate, 1) should be (0.311111 +- 0.00001))

  "30E/360" should "return 60 as number of days in delta quants case 1" in
    (M30EY360.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (60))

  "30E/360" should "return 0.166666666666667 as day-fraction" in
    (M30EY360.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.166666666666667 +- 0.00001))

  "30E/360" should "return 61 as number of days in delta quants case 1" in
    (M30EY360.countDays(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate) should be (61))

  "30E/360" should "return 0.169444444444444 as day-fraction" in
    (M30EY360.accruedFactor(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate, deltaQuantsEx2EndDate, 1) should be (0.169444444444444 +- 0.00001))

  "30E/360" should "return 390 as number of days in delta quants case 1" in
    (M30EY360.countDays(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate) should be (390))

  "30E/360" should "return 1.108333333333333 as day-fraction" in
    (M30EY360.accruedFactor(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate, deltaQuantsEx3EndDate, 1) should be (1.108333333333333 +- 0.00001))

  "30E/360" should "return 479 as number of days in delta quants case 1" in
    (M30EY360.countDays(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate) should be (479))

  "30E/360" should "return 1.33055555555556 as day-fraction" in
    (M30EY360.accruedFactor(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate, deltaQuantsEx4EndDate, 1) should be (1.33055555555556 +- 0.00001))


  // 30E+/360
  "30E+/360" should "return 60 as number of days in delta quants case 1" in
    (M30EPlusY360.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (60))

  "30E+/360" should "return 0.166666666666667 as day-fraction in delta quants case 1" in
    (M30EPlusY360.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.166666666666667 +- 0.00001))

  "30E+/360" should "return 61 as number of days in delta quants case 1" in
    (M30EPlusY360.countDays(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate) should be (61))

  "30E+/360" should "return 0.169444444444444 as day-fraction" in
    (M30EPlusY360.accruedFactor(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate, deltaQuantsEx2EndDate, 1) should be (0.169444444444444 +- 0.00001))

  "30E+/360" should "return 390 as number of days in delta quants case 1" in
    (M30EPlusY360.countDays(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate) should be (390))

  "30E+/360" should "return 1.108333333333333 as day-fraction" in
    (M30EPlusY360.accruedFactor(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate, deltaQuantsEx3EndDate, 1) should be (1.108333333333333 +- 0.00001))

  "30E+/360" should "return 480 as number of days in delta quants case 1" in
    (M30EPlusY360.countDays(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate) should be (480))

  "30E+/360" should "return 1.33333333333333 as day-fraction" in
    (M30EPlusY360.accruedFactor(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate, deltaQuantsEx4EndDate, 1) should be (1.33333333333333 +- 0.00001))

  // 30/360 German
  "30/360 German" should "return 60 as number of days in delta quants case 1" in
    (M30Y360German.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (60))

  "30/360 German" should "return 0.166666666666667 as day-fraction in delta quants case 1" in
    (M30Y360German.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.166666666666667 +- 0.00001))

  "30/360 German" should "return 62 as number of days in delta quants case 1" in
    (M30Y360German.countDays(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate) should be (62))

  "30/360 German" should "return 0.172222222222222 as day-fraction" in
    (M30Y360German.accruedFactor(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate, deltaQuantsEx2EndDate, 1) should be (0.172222222222222 +- 0.00001))

  "30/360 German" should "return 390 as number of days in delta quants case 1" in
    (M30Y360German.countDays(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate) should be (390))

  "30/360 German" should "return 1.108333333333333 as day-fraction" in
    (M30Y360German.accruedFactor(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate, deltaQuantsEx3EndDate, 1) should be (1.108333333333333 +- 0.00001))

  "30/360 German" should "return 479 as number of days in delta quants case 1" in
    (M30Y360German.countDays(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate) should be (479))

  "30/360 German" should "return 1.33055555555556 as day-fraction" in
    (M30Y360German.accruedFactor(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate, deltaQuantsEx4EndDate, 1) should be (1.33055555555556 +- 0.00001))


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

  "Actual/365 Fixed" should "return 63 as number of days in delta quants case 1" in
    (Actual365Fixed.countDays(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate) should be (63))

  "Actual/365 Fixed" should "return 0.172602739726027 as day-fraction" in
    (Actual365Fixed.accruedFactor(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate, deltaQuantsEx2EndDate, 1) should be (0.172602739726027 +- 0.00001))

  "Actual/365 Fixed" should "return 396 as number of days in delta quants case 1" in
    (Actual365Fixed.countDays(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate) should be (396))

  "Actual/365 Fixed" should "return 1.08493150684932 as day-fraction" in
    (Actual365Fixed.accruedFactor(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate, deltaQuantsEx3EndDate, 1) should be (1.08493150684932 +- 0.00001))

  "Actual/365 Fixed" should "return 485 as number of days in delta quants case 1" in
    (Actual365Fixed.countDays(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate) should be (485))

  "Actual/365 Fixed" should "return 1.32876712328767 as day-fraction" in
    (Actual365Fixed.accruedFactor(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate, deltaQuantsEx4EndDate, 1) should be (1.32876712328767 +- 0.00001))


  // NL/365
  "NL/365" should "return 62 as number of days in delta quants case 1" in
    (NL365.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (62))

  "NL/365" should "return 0.16986301369863 as day-fraction in delta quants case 1" in
    (NL365.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.16986301369863 +- 0.00001))

  "NL/365" should "return 62 as number of days in delta quants case 1" in
    (NL365.countDays(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate) should be (62))

  "NL/365" should "return 0.16986301369863 as day-fraction" in
    (NL365.accruedFactor(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate, deltaQuantsEx2EndDate, 1) should be (0.16986301369863 +- 0.00001))

  "NL/365" should "return 395 as number of days in delta quants case 1" in
    (NL365.countDays(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate) should be (395))

  "NL/365" should "return 1.08219178082192 as day-fraction" in
    (NL365.accruedFactor(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate, deltaQuantsEx3EndDate, 1) should be (1.08219178082192 +- 0.00001))

  "NL/365" should "return 484 as number of days in delta quants case 1" in
    (NL365.countDays(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate) should be (484))

  "NL/365" should "return 1.32602739726027 as day-fraction" in
    (NL365.accruedFactor(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate, deltaQuantsEx4EndDate, 1) should be (1.32602739726027 +- 0.00001))



  //Actual/Actual ISDA (Act/Act)
  ignore should "return 0.770492 as day-fraction" in
    (ActualActualISDA.accruedFactor(inYearNoLeapStartDate, inYearNoLeapCurrentDate, inYearNoLeapEndDate, 1) should be (0.770492 +- 0.00001))

  ignore should "return 0.308743 as day-fraction" in
    (ActualActualISDA.accruedFactor(crossYearWithLeapStartDate, crossYearWithLeapCurrentDate, crossYearWithLeapEndDate, 1) should be (0.308743 +- 0.00001))

  "Actual/Actual ISDA" should "return 0.16942884946478 as day-fraction in delta quants case 1" in
    (ActualActualISDA.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.16942884946478 +- 0.00001))

  "Actual/Actual ISDA" should "return 0.172161089901939 as day-fraction" in
    (ActualActualISDA.accruedFactor(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate, deltaQuantsEx2EndDate, 1) should be (0.172161089901939 +- 0.00001))

  "Actual/Actual ISDA" should "return 1.08243131970956 as day-fraction" in
    (ActualActualISDA.accruedFactor(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate, deltaQuantsEx3EndDate, 1) should be (1.08243131970956 +- 0.00001))

  "Actual/Actual ISDA" should "return 1.32625945055768 as day-fraction" in
    (ActualActualISDA.accruedFactor(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate, deltaQuantsEx4EndDate, 1) should be (1.32625945055768 +- 0.00001))


  // 30/360 German (30E/360 ISDA)
  "30E/360 ISDA" should "return 60 as number of days in delta quants case 1" in
    (M30EY360ISDA.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (60))

  "30E/360 ISDA" should "return 0.166666666666667 as day-fraction" in
    (M30EY360ISDA.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.166666666666667 +- 0.00001))

  "30E/360 ISDA" should "return 61 as number of days in delta quants case 1" in
    (M30EY360ISDA.countDays(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate) should be (61))

  "30E/360 ISDA" should "return 0.169444444444444 as day-fraction" in
    (M30EY360ISDA.accruedFactor(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate, deltaQuantsEx2EndDate, 1) should be (0.169444444444444 +- 0.00001))

  "30E/360 ISDA" should "return 390 as number of days in delta quants case 1" in
    (M30EY360ISDA.countDays(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate) should be (390))

  "30E/360 ISDA" should "return 1.08333333333333 as day-fraction" in
    (M30EY360ISDA.accruedFactor(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate, deltaQuantsEx3EndDate, 1) should be (1.08333333333333 +- 0.00001))

  "30E/360 ISDA" should "return 480 as number of days in delta quants case 1" in
    (M30EY360ISDA.countDays(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate) should be (480))

  "30E/360 ISDA" should "return 1.33333333333333 as day-fraction" in
    (M30EY360ISDA.accruedFactor(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate, deltaQuantsEx4EndDate, 1) should be (1.33333333333333 +- 0.00001))


  // Actual/365A
  "Actual/365A" should "return 62 as number of days in delta quants case 1" in
    (Actual365A.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (62))

  "Actual/365A" should "return 0.16986301369863 as day-fraction in delta quants case 1" in
    (Actual365A.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.16986301369863 +- 0.00001))

  "Actual/365A" should "return 63 as number of days in delta quants case 1" in
    (Actual365A.countDays(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate) should be (63))

  "Actual/365A" should "return 0.172131147540984 as day-fraction" in
    (Actual365A.accruedFactor(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate, deltaQuantsEx2EndDate, 1) should be (0.172131147540984 +- 0.00001))

  "Actual/365A" should "return 396 as number of days in delta quants case 1" in
    (Actual365A.countDays(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate) should be (396))

  "Actual/365A" should "return 1.08196721311475 as day-fraction" in
    (Actual365A.accruedFactor(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate, deltaQuantsEx3EndDate, 1) should be (1.08196721311475 +- 0.00001))

  "Actual/365A" should "return 485 as number of days in delta quants case 1" in
    (Actual365A.countDays(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate) should be (485))

  "Actual/365A" should "return 1.32513661202186 as day-fraction" in
    (Actual365A.accruedFactor(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate, deltaQuantsEx4EndDate, 1) should be (1.32513661202186 +- 0.00001))


  // Actual/360 (Act/360)
  "Actual/360" should "return 62 as number of days in delta quants case 1" in
    (Actual360.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (62))

  "Actual/360" should "return 0.172222222222222 as day-fraction in delta quants case 1" in
    (Actual360.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.172222222222222 +- 0.00001))

  "Actual/360" should "return 63 as number of days in delta quants case 1" in
    (Actual360.countDays(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate) should be (63))

  "Actual/360" should "return 0.175 as day-fraction" in
    (Actual360.accruedFactor(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate, deltaQuantsEx2EndDate, 1) should be (0.175 +- 0.00001))

  "Actual/360" should "return 396 as number of days in delta quants case 1" in
    (Actual360.countDays(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate) should be (396))

  "Actual/360" should "return 1.1000000000000 as day-fraction" in
    (Actual360.accruedFactor(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate, deltaQuantsEx3EndDate, 1) should be (1.1000000000000 +- 0.00001))

  "Actual/360" should "return 485 as number of days in delta quants case 1" in
    (Actual360.countDays(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate) should be (485))

  "Actual/360" should "return 1.34722222222222 as day-fraction" in
    (Actual360.accruedFactor(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate, deltaQuantsEx4EndDate, 1) should be (1.34722222222222 +- 0.00001))

  // Act/365L
  "Actual/365L" should "return 62 as number of days in delta quants case 1" in
    (Actual365L.countDays(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate) should be (62))

  "Actual/365L" should "return 0.169398907103825 as day-fraction in delta quants case 1" in
    (Actual365L.accruedFactor(deltaQuantsEx1StartDate, deltaQuantsEx1CurrentDate, deltaQuantsEx1EndDate, 1) should be (0.169398907103825 +- 0.00001))


  "Actual/365L" should "return 63 as number of days in delta quants case 1" in
    (Actual365L.countDays(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate) should be (63))

  "Actual/365L" should "return 0.172131147540984 as day-fraction" in
    (Actual365L.accruedFactor(deltaQuantsEx2StartDate, deltaQuantsEx2CurrentDate, deltaQuantsEx2EndDate, 1) should be (0.172131147540984 +- 0.00001))

  "Actual/365L" should "return 396 as number of days in delta quants case 1" in
    (Actual365L.countDays(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate) should be (396))

  "Actual/365L" should "return 1.08196721311475 as day-fraction" in
    (Actual365L.accruedFactor(deltaQuantsEx3StartDate, deltaQuantsEx3CurrentDate, deltaQuantsEx3EndDate, 1) should be (1.08196721311475 +- 0.00001))

  "Actual/365L" should "return 485 as number of days in delta quants case 1" in
    (Actual365L.countDays(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate) should be (485))

  "Actual/365L" should "return 1.32876712328767 as day-fraction" in
    (Actual365L.accruedFactor(deltaQuantsEx4StartDate, deltaQuantsEx4CurrentDate, deltaQuantsEx4EndDate, 1) should be (1.32876712328767 +- 0.00001))





