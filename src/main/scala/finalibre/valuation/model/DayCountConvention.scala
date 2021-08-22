package finalibre.valuation.model

import java.time.temporal.{ChronoUnit, TemporalAdjusters, TemporalUnit}
import java.time.{LocalDate, Month}

trait DayCountConvention:
  def countDays(periodStart : LocalDate, currentDate : LocalDate) : Int
  def denominator(periodStart : LocalDate, periodEnd : LocalDate, frequency : Int) : Int
  def accruedFactor(periodStart : LocalDate, currentDate : LocalDate, periodEnd : LocalDate, frequency : Int) : Double = countDays(periodStart, currentDate).toDouble / denominator(periodStart, periodEnd, frequency)


object DayCountConvention:

  abstract class M30Y360 extends DayCountConvention:
    def dayPartOf(d1 : LocalDate, d2 : LocalDate) : (Int, Int)
    override def denominator(periodStart : LocalDate, periodEnd : LocalDate, frequency : Int) = 360
    override def countDays(periodStart: LocalDate, currentDate: LocalDate): Int =
      val (d1, d2) = dayPartOf(periodStart, currentDate)
      (360 * (currentDate.getYear - periodStart.getYear) + 30 * (currentDate.getMonthValue - periodStart.getMonthValue) + (d2 - d1))


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

  object M30EY360 extends M30Y360:
    override def dayPartOf(d1: LocalDate, d2: LocalDate): (Int, Int) =
      var rDay1 = math.min(d1.getDayOfMonth,30)
      var rDay2 = math.min(d2.getDayOfMonth,30)
      (rDay1, rDay2)

  object M30EY360ISDA extends M30Y360:
    override def dayPartOf(d1: LocalDate, d2: LocalDate): (Int, Int) =
      var rDay1 = math.min(d1.getDayOfMonth,30)
      var rDay2 = math.min(d2.getDayOfMonth,30)
      (rDay1, rDay2)
    def countDaysOnMatDate(periodStart : LocalDate, periodEndDate : LocalDate) =
      val d1 = math.min(periodStart.getDayOfMonth, 30)
      val d2 = if periodEndDate.getMonth == Month.FEBRUARY then periodEndDate.getDayOfMonth else math.min(periodEndDate.getDayOfMonth, 30)
      (360 * (periodEndDate.getYear - periodStart.getYear) + 30 * (d2 - d1) + (d2 - d1))

  object ActualActualISMA extends DayCountConvention:
    override def countDays(periodStart: LocalDate, currentDate: LocalDate): Int = ChronoUnit.DAYS.between(periodStart, currentDate).toInt
    override def denominator(periodStart: LocalDate, periodEnd: LocalDate, frequency: Int): Int = frequency * ChronoUnit.DAYS.between(periodStart, periodEnd).toInt

  object ActualActualISDA extends DayCountConvention:
    override def countDays(periodStart: LocalDate, currentDate: LocalDate): Int = -1
    override def denominator(periodStart: LocalDate, periodEnd: LocalDate, frequency: Int): Int = -1
    override def accruedFactor(periodStart: LocalDate, currentDate: LocalDate, periodEnd: LocalDate, frequency: Int): Double =
      (periodStart.getYear, currentDate.getYear) match
        case (y1, y2) if y1 == y2 => 
          (ChronoUnit.DAYS.between(periodStart, currentDate)).toDouble / (if periodStart.isLeapYear then 366 else 365)
        case (y1, y2) if y1 < y2 =>
          val daysInY1 = ChronoUnit.DAYS.between(periodStart,periodStart.`with`(TemporalAdjusters.lastDayOfYear())).toInt
          val daysInY2 = ChronoUnit.DAYS.between(currentDate.`with`(TemporalAdjusters.firstDayOfYear()).minusDays(1), currentDate).toInt
          val (leapYearDays, nonLeapYearDays) = Range(1, y2 - y1).foldLeft((0,0)) {
            case ((inLeaps, notInLeaps), plusYears) => if LocalDate.of(y1 + plusYears, 1, 1).isLeapYear then (inLeaps + 366, notInLeaps) else (inLeaps, notInLeaps + 365)
          }
          val (y1Leap, y1NonLeap) = if periodStart.isLeapYear then (daysInY1, 0) else (0, daysInY1)
          val (y2Leap, y2NonLeap) = if currentDate.isLeapYear then (daysInY2, 0) else (0, daysInY2)
          (y1Leap + y2Leap + leapYearDays).toDouble/366.0 + (y1NonLeap + y2NonLeap + nonLeapYearDays).toDouble / 365.0

  object Actual365Fixed extends DayCountConvention:
    override def countDays(periodStart: LocalDate, currentDate: LocalDate): Int = ChronoUnit.DAYS.between(periodStart, currentDate).toInt
    override def denominator(periodStart: LocalDate, periodEnd: LocalDate, frequency: Int): Int = 365

  object Actual360 extends DayCountConvention:
    override def countDays(periodStart: LocalDate, currentDate: LocalDate): Int = ChronoUnit.DAYS.between(periodStart, currentDate).toInt
    override def denominator(periodStart: LocalDate, periodEnd: LocalDate, frequency: Int): Int = 360

  object Actual364 extends DayCountConvention:
    override def countDays(periodStart: LocalDate, currentDate: LocalDate): Int = ChronoUnit.DAYS.between(periodStart, currentDate).toInt
    override def denominator(periodStart: LocalDate, periodEnd: LocalDate, frequency: Int): Int = 364

  object Actual365A extends DayCountConvention:
    override def countDays(periodStart: LocalDate, currentDate: LocalDate): Int = ChronoUnit.DAYS.between(periodStart, currentDate).toInt
    override def denominator(periodStart: LocalDate, periodEnd: LocalDate, frequency: Int): Int =
      val theNextFeb29 = nextFeb29(periodStart)
      if theNextFeb29.isBefore(periodEnd) then 366 else 365

    override def accruedFactor(periodStart: LocalDate, currentDate: LocalDate, periodEnd: LocalDate, frequency: Int): Double =
      val numerator = countDays(periodStart, currentDate)
      val denom = denominator(periodStart, currentDate, frequency)
      numerator.toDouble / denom.toDouble



  object Actual365L extends DayCountConvention:
    override def countDays(periodStart: LocalDate, currentDate: LocalDate): Int = ChronoUnit.DAYS.between(periodStart, currentDate).toInt
    override def denominator(periodStart: LocalDate, periodEnd: LocalDate, frequency: Int): Int =
      val theNextFeb29 = nextFeb29(periodStart)
      frequency match
        case 1 if theNextFeb29.isBefore(periodEnd) => 366
        case 1 => 365
        case _ if periodEnd.isLeapYear => 366
        case _ => 365

  object NL365 extends DayCountConvention:
    override def countDays(periodStart: LocalDate, currentDate: LocalDate): Int =
      val actualNumberOfDays = ChronoUnit.DAYS.between(periodStart, currentDate).toInt
      val theNextFeb29 = nextFeb29(periodStart)
      (if theNextFeb29.isBefore(currentDate) then -1 else 0) + actualNumberOfDays

    override def denominator(periodStart: LocalDate, periodEnd: LocalDate, frequency: Int): Int = 365


  object ActualActualAFB extends DayCountConvention:
    type SplitPeriod = (Option[Int], (LocalDate, LocalDate, Int))
    def splitPeriod(periodStart : LocalDate, periodEnd : LocalDate) : SplitPeriod =
      val theNextFeb29 = nextFeb29(periodStart)
      (ChronoUnit.DAYS.between(periodStart, periodEnd), theNextFeb29.isBefore(periodEnd)) match
        case (per,true) if per <= 366  => (None, (periodStart, periodEnd, 366))
        case (per,false) if per <= 365  => (None, (periodStart, periodEnd, 366))
        case per =>
          val yearsBetween = ChronoUnit.YEARS.between(periodStart, periodEnd)
          val splitDate = periodEnd.minus(yearsBetween, ChronoUnit.YEARS)
          val remainderDenom = if theNextFeb29.isBefore(splitDate) then 366 else 365
          (Some(yearsBetween.toInt), (periodStart, splitDate, remainderDenom))

    override def countDays(periodStart: LocalDate, currentDate: LocalDate): Int =
      val (_,(_,spltDay,_)) = splitPeriod(periodStart, currentDate)
      ChronoUnit.DAYS.between(periodStart, spltDay).toInt
    override def denominator(periodStart: LocalDate, periodEnd: LocalDate, frequency: Int): Int =
      val (_,(_,_,denom)) = splitPeriod(periodStart, periodEnd)
      denom

    override def accruedFactor(periodStart: LocalDate, currentDate: LocalDate, periodEnd: LocalDate, frequency: Int): Double =
      splitPeriod(periodStart, periodEnd) match
        case (yearsOpt, (from, to, denom)) =>
          yearsOpt.map(_.toDouble).getOrElse(0.0) + ChronoUnit.DAYS.between(from, to).toDouble / denom.toDouble

  object OneOne extends DayCountConvention:
    override def countDays(periodStart: LocalDate, currentDate: LocalDate): Int = ChronoUnit.DAYS.between(periodStart, currentDate).toInt

    override def denominator(periodStart: LocalDate, periodEnd: LocalDate, frequency: Int): Int = 365

    override def accruedFactor(periodStart: LocalDate, currentDate: LocalDate, periodEnd: LocalDate, frequency: Int): Double =
      countDays(periodStart, currentDate).toDouble / 365.25




  def nextFeb29(fromDate : LocalDate) =
    fromDate.isLeapYear match
      case true if LocalDate.of(fromDate.getYear, 2, 29).isAfter(fromDate) => LocalDate.of(fromDate.getYear, 2, 29)
      case _ =>
        val toAdd = Range(1,10).find(addee => LocalDate.of(fromDate.getYear + addee, 1,1).isLeapYear).get
        LocalDate.of(fromDate.getYear + toAdd, 2, 29)


/*  case , ,M30EY360ISDA,
  ActualAcualICMA, ActualActualISDA, ActualY365Fixed*/