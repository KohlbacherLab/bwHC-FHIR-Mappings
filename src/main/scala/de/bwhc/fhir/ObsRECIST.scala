package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Observation._
import org.hl7.fhir.r4.json._

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.RECIST


sealed trait ObsRECISTProfile
extends ObservationSC
   with Observation.identifierNel
   with Observation.partOfNel[MedicationStatement]
   with Observation.effectiveDateTime[LocalDate,Required]
   with Observation.subject[MTBPatient,Required]
   with Observation.valueCodeableConcept[
     CodeableConcept with CodeableConcept.codingNel[Coding[RECIST.Value]],
     Required
   ]


case class ObsRECIST
(
  identifier: NonEmptyList[Identifier],
  status: Observation.Status.Value,
  partOf: NonEmptyList[LogicalReference[MedicationStatement]],
  effectiveDateTime: LocalDate,
  subject: LogicalReference[MTBPatient],
  valueCodeableConcept: BasicCodeableConcept[RECIST.Value]
)
extends ObsRECISTProfile


object ObsRECIST
{

  import CodingSystems._

  implicit val profiles =
    Meta.Profiles[ObsRECIST]("http://bwhc-mtb-therapy-response")
    
  implicit val code = Code[ObsRECIST](LOINC("21976-6"))

  implicit val format = Json.format[ObsRECIST]
  
}
