package nl.surfnet.safnari

import javax.xml.datatype.XMLGregorianCalendar
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class PackageSpec extends helpers.Specification {
  object XmlGregorianCalendar {
    lazy val factory = javax.xml.datatype.DatatypeFactory.newInstance()

    def apply(date: String): XMLGregorianCalendar = factory.newXMLGregorianCalendar(date)
  }

  implicit val arbitraryXmlGregorianCalendar = {
    Arbitrary(for {
      timeInMillis <- Gen.choose(0, System.currentTimeMillis() * 3)
      timezoneOffset <- Gen.choose(-14 * 60, 14 * 60)
    } yield {
      val dt = new DateTime(timeInMillis)
      XmlGregorianCalendar.factory.newXMLGregorianCalendar(
        dt.getYear(), dt.getMonthOfYear(), dt.getDayOfMonth(),
        dt.getHourOfDay(), dt.getMinuteOfHour(), dt.getSecondOfMinute(), dt.getMillisOfSecond(),
        timezoneOffset)
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
      val dates = List(XmlGregorianCalendar("2002-10-09T11:00:00"), XmlGregorianCalendar("2001-10-09T11:00:00"))

      dates.max must beEqualTo(XmlGregorianCalendar("2002-10-09T11:00:00"))
    }
  }

  "XMLGregorianCalendar and DateTime" should {
    "convert arbitrary XMLGregorianCalendar to DateTime and back" in prop { (xmlDateTime: XMLGregorianCalendar) =>
      xmlDateTime.toDateTime.toXmlGregorianCalendar must_== xmlDateTime
    }.set(minTestsOk = 1000, workers = 4)

    "convert from DateTime to XMLGregorianCalendar" in {
      val actual = new DateTime(2013, 11, 29, 11, 33, 7, 23, DateTimeZone.forOffsetHoursMinutes(3, 30)).toXmlGregorianCalendar
      val expect = XmlGregorianCalendar("2013-11-29T11:33:07.023+03:30")
      actual must_== expect
    }

    "convert from XMLGregorianCalendar to DateTime" in {
      val actual = XmlGregorianCalendar("2013-11-29T11:33:07.023+03:30").toDateTime
      val expect = new DateTime(2013, 11, 29, 11, 33, 7, 23, DateTimeZone.forOffsetHoursMinutes(3, 30))
      actual must_== expect
    }
  }
}
