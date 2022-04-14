package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Observation._
import org.hl7.fhir.r4.json._

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.ECOG


import CodingSystems._



trait ObsECOGProfile
extends ObservationSC
   with Observation.identifierNel
   with Observation.effectiveDateTime[LocalDate,Optional]
   with Observation.subject[MTBPatient,Required]
   with Observation.valueCodeableConcept[
     CodeableConcept with CodeableConcept.codingNel[CodingStatic[ECOG.Value]],
     Required
   ]


case class ObsECOG
(
  identifier: NonEmptyList[Identifier],
  status: Observation.Status.Value,
  effectiveDateTime: Option[LocalDate],
  subject: LogicalReference[MTBPatient],
  valueCodeableConcept: CodeableConceptStatic[ECOG.Value]
)
extends ObsECOGProfile


object ObsECOG
{

  implicit val profiles = Meta.Profiles[ObsECOG]("http://bwhc.de/obs-ecog-performance-status")
    
    
  implicit val code = Code[ObsECOG](LOINC("89247-1"))

  implicit val formatObsECOG = Json.format[ObsECOG]
  
}
