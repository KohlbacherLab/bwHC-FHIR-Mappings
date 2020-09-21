package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Observation._
import org.hl7.fhir.r4.Patient._
import org.hl7.fhir.r4.json._

import play.api.libs.json.Json


import de.bwhc.mtb.data.entry.dtos.{
  ICD10GM,
  ICDO3M
}

import CodingSystems._



trait ObsHistologyProfile
extends Observation
   with Observation.identifierNel
   with Observation.effectiveDateTime[LocalDate,Optional]
   with Observation.subject[MTBPatient,Required]
   with Observation.valueCodeableConcept[
          CodeableConcept with CodeableConcept.codingNel[Coding[ICDO3M]],
          Optional
        ]
   with Observation.specimen[Required]
   with Observation.note[Annotation,Optional]


object ObsHistologyProfile
{

  implicit def code[O <: ObsHistology] = LOINC.Code[O]("59847-4")

}



final case class ObsHistology
(
  identifier: NonEmptyList[Identifier],
  status: Observation.Status.Value,
  effectiveDateTime: Option[LocalDate],
  subject: LogicalReference[MTBPatient],
  specimen: LogicalReference[Specimen],
//  subject: Reference[MTBPatient],
//  specimen: Reference[Specimen],
  valueCodeableConcept: Option[BasicCodeableConcept[ICDO3M]],
  note: Option[List[Protocol]]
)
extends ObsHistologyProfile

case class Protocol(text: String) extends Annotation
object Protocol
{
  implicit val format = Json.format[Protocol]
}



object ObsHistology
{

  implicit val profile =
    Meta.Profiles[ObsHistology]("http://de.bwhc/histology")
    
//  implicit val code = LOINC.Code[ObsHistology]("59847-4")

  import ObsHistologyProfile._

  implicit val formatObsHistology = Json.format[ObsHistology]
  
}
