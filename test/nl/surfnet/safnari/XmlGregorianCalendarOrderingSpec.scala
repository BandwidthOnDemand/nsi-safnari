package nl.surfnet.safnari

import javax.xml.datatype.DatatypeFactory
import javax.xml.datatype.XMLGregorianCalendar

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class XmlGregorianCalendarOrderingSpec extends helpers.Specification {

  "Xml Gregorian Calender order" should {

    "sort a list of calendars" in {
      val date1 = XmlGregorianCalendar("2001-09-09-11:00")
      val date2 = XmlGregorianCalendar("2001-10-08-11:00")
      val date3 = XmlGregorianCalendar("2001-10-09-10:00")
      val date4 = XmlGregorianCalendar("2001-10-09-11:00")
      val date5 = XmlGregorianCalendar("2002-10-09-11:00")

      val sorted = List(date3, date5, date2, date1, date4).sorted

      sorted must contain(date1, date2, date3, date4, date5).inOrder
    }

    "find max in" in {
      val dates = List(XmlGregorianCalendar("2002-10-09-11:00"), XmlGregorianCalendar("2001-10-09-11:00"))
      val maxDate = dates.max

      maxDate must beEqualTo(XmlGregorianCalendar("2002-10-09-11:00"))
    }
  }

  object XmlGregorianCalendar {
    lazy val factory = DatatypeFactory.newInstance()

    def apply(date: String): XMLGregorianCalendar = factory.newXMLGregorianCalendar(date)
  }
}
