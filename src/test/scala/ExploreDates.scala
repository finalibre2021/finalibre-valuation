import java.time.LocalDate
import java.time.temporal.ChronoUnit

object ExploreDates

  @main def main(): Unit =
    val fromDate = LocalDate.of(2020,4,3)
    val toDate = LocalDate.of(2021,4,2)
    val years = ChronoUnit.YEARS.between(fromDate, toDate)
    println(s"Hay ${years} años entre $fromDate y $toDate")


