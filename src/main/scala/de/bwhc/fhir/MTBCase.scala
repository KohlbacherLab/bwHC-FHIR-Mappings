package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Encounter._

import play.api.libs.json.Json



trait MTBCaseProfile
extends Encounter
   with Encounter.identifierNel
   with Encounter.subject[Patient,Required]
   with Encounter.diagnosisNel[Encounter.DiagnosisElement[DiagnosisProfile]]

/*
case class MTBCase
(
  identifier: NonEmptyList[Identifier],
  status: Encounter.Status.Value,
  `class`: BasicCoding[HL7v3ActEncounterCode.Value],
  subject: LogicalReference[MTBPatient],
  diagnosis: NonEmptyList[MTBCase.Diagnosis]
)
extends MTBCaseProfile


object MTBCase
{

  implicit val profiles = Meta.Profiles[MTBCase]("http://bwhc-mtb-case")
    
    
  final case class Diagnosis(
    condition: LogicalReference[DiagnosisProfile]
  ) extends Encounter.DiagnosisElement[DiagnosisProfile]


  import org.hl7.fhir.r4.Condition._
  import org.hl7.fhir.r4.Patient._
  import org.hl7.fhir.r4.json._

  implicit val formatDiagnosis = Json.format[Diagnosis]
    
  implicit val format = Json.format[MTBCase]
  
}
*/
