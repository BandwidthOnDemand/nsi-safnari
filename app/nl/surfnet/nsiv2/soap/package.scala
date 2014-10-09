package nl.surfnet.nsiv2

import utils._
import org.ogf.schemas.nsi._2013._12.connection.types.{ReservationRequestCriteriaType, ReservationConfirmCriteriaType}

import scala.util.{Try, Success, Failure}

package object soap {
  implicit class AnyOps[A](a: A) {
    def tap(f: A => Unit): A = { f(a); a }
  }

  implicit val ReservationCriteriaConversion = Conversion.build[ReservationConfirmCriteriaType, ReservationRequestCriteriaType] { a =>
    Try(new ReservationRequestCriteriaType().
      withSchedule(a.getSchedule).
      withAny(a.getAny).
      withServiceType(a.getServiceType).
      withVersion(a.getVersion).
      tap(_.getOtherAttributes.putAll(a.getOtherAttributes)))
  } { b =>
    for {
      schedule <- Option(b.getSchedule).map(Success(_)).getOrElse(Failure(ErrorMessage("schedule is required")))
      serviceType <- Option(b.getServiceType).map(Success(_)).getOrElse(Failure(ErrorMessage("serviceType is required")))
    } yield {
      new ReservationConfirmCriteriaType().
        withSchedule(schedule).
        withAny(b.getAny).
        withServiceType(b.getServiceType).
        withVersion(if (b.getVersion == null) 1 else b.getVersion).
        tap(_.getOtherAttributes.putAll(b.getOtherAttributes))
    }
  }
}
