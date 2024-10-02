package nl.surfnet.safnari

import nl.surfnet.nsiv2.utils._

import javax.xml.datatype.XMLGregorianCalendar
import java.time.{Instant, ZoneOffset}
import java.time.temporal._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PackageSpec extends helpers.Specification {
  object XmlGregorianCalendar {
    lazy val factory = javax.xml.datatype.DatatypeFactory.newInstance()

    def apply(date: String): XMLGregorianCalendar = factory.newXMLGregorianCalendar(date)
  }

  implicit val arbitraryXmlGregorianCalendar: Arbitrary[XMLGregorianCalendar] = {
    Arbitrary(for {
      timeInMillis <- Gen.choose(0, System.currentTimeMillis() * 3)
      timezoneOffset <- Gen.choose(-14 * 60, 14 * 60)
    } yield {
      val dt = Instant.ofEpochMilli(timeInMillis).atOffset(ZoneOffset.ofHours(timezoneOffset))
      XmlGregorianCalendar.factory.newXMLGregorianCalendar(
        dt.getYear(),
        dt.getMonthValue() - 1,
        dt.getDayOfMonth(),
        dt.getHour(),
        dt.getMinute(),
        dt.getSecond(),
        dt.get(ChronoField.MILLI_OF_SECOND),
        dt.getOffset().getTotalSeconds() / 60
      )
    })
  }

  "XML Gregorian Calender order" should {
    "sort a list of calendars" in {
      val date1 = XmlGregorianCalendar("2001-09-09T11:00:00")
      val date2 = XmlGregorianCalendar("2001-10-08T11:00:00")
      val date3 = XmlGregorianCalendar("2001-10-09T10:00:00")
      val date4 = XmlGregorianCalendar("2001-10-09T11:00:00")
      val date5 = XmlGregorianCalendar("2002-10-09T11:00:00")

      val sorted = List(date3, date5, date2, date1, date4).sorted

      sorted must containAllOf(Seq(date1, date2, date3, date4, date5)).inOrder
    }

    "find max in" in {
      val dates = List(
        XmlGregorianCalendar("2002-10-09T11:00:00"),
        XmlGregorianCalendar("2001-10-09T11:00:00")
      )

      dates.max must beEqualTo(XmlGregorianCalendar("2002-10-09T11:00:00"))
    }
  }
}
