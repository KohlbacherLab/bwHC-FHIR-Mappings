package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._

import play.api.libs.json.Json


trait BwHCConsentProfile
extends Consent
   with Consent.identifierNel
   with Consent.patient[Required]


final case class BwHCConsent
(
  identifier: NonEmptyList[Identifier],
  status: Consent.Status.Value,
  patient: Reference[MTBPatient],
  scope: BasicCodeableConcept[Consent.Scope.Value]
)
extends BwHCConsentProfile


object BwHCConsent
{

  implicit val profiles = Meta.Profiles[BwHCConsent]("http://bwhc.de/consent")
    

  import org.hl7.fhir.r4.json._

    
  implicit val format = Json.format[BwHCConsent]
  
}
