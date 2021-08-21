package finalibre.valuation.model

import java.time.{LocalDate, Month}

trait DayCountConvention:
  def countDays(periodStart : LocalDate, currentDate : LocalDate) : Int
  def denominator(periodStart : LocalDate, periodEnd : LocalDate) : Int
  def accruedFactor(periodStart : LocalDate, currentDate : LocalDate, periodEnd : LocalDate) : Double = countDays(periodStart, currentDate).toDouble / denominator(periodStart, periodEnd)


object DayCountConvention:

  abstract class M30Y360 extends DayCountConvention:
    def dayPartOf(d1 : LocalDate, d2 : LocalDate) : (Int, Int)
    override def denominator(periodStart : LocalDate, periodEnd : LocalDate) = 360
    override def countDays(periodStart: LocalDate, currentDate: LocalDate): Int =
      val (d1, d2) = dayPartOf(periodStart, currentDate)
      (360 * (currentDate.getYear - periodStart.getYear) + 30 * (d2 - d1) + (d2 - d1))


  object M30Y360BondBasis extends M30Y360:
    override def dayPartOf(d1: LocalDate, d2: LocalDate): (Int, Int) =
      var rDay1 = math.min(d1.getDayOfMonth,30)
      var rDay2 = if rDay1 > 29 then math.min(d2.getDayOfMonth,30) else d2.getDayOfMonth
      if rDay2 == 31 && rDay1 >= 30 then
        rDay2 = 30
      if rDay1 == 31 then
        rDay1 = 30
      (rDay1, rDay2)

  object M30Y360US extends M30Y360:
    override def dayPartOf(d1: LocalDate, d2: LocalDate): (Int, Int) =
      var rDay1 = math.min(d1.getDayOfMonth,30)
      var rDay2 = if d2.getDayOfMonth == 31 && rDay1 >= 30 then 30 else d2.getDayOfMonth
      val rDay1onEndOfFeb = if d1.getMonth == Month.FEBRUARY && ((rDay1 == 28 && !d1.isLeapYear) || (rDay1 == 29)) then true else false
      if rDay1onEndOfFeb then
        rDay1 = 30
        val rDay2onEndOfFeb = if d2.getMonth == Month.FEBRUARY && ((rDay2 == 28 && !d2.isLeapYear) || (rDay2 == 29)) then true else false
        if rDay2onEndOfFeb then
          rDay2 = 30
      (rDay1, rDay2)





/*  case , , M30EY360, M30EY360ISDA,
  ActualAcualICMA, ActualActualISDA, ActualY365Fixed*/