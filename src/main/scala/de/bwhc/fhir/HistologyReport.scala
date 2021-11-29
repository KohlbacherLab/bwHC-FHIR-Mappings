package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList


import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Observation._
import org.hl7.fhir.r4.DiagnosticReport._
import org.hl7.fhir.r4.json._

import play.api.libs.json.Json


import de.bwhc.mtb.data.entry.dtos.{
  ICDO3M,
  TumorCellContent
}


//-----------------------------------------------------------------------------
// Tumor Morphology
//-----------------------------------------------------------------------------

abstract class ObsTumorMorphologyProfile
//extends Observation
extends ObservationSC
   with Observation.id[Required]
   with Observation.subject[Patient,Required]
   with Observation.specimen[Required]
   with Observation.valueCodeableConcept[
     CodeableConcept with CodeableConcept.codingNel[CodingStatic[ICDO3M]],
     Required
   ]
   with Observation.note[Annotation,Optional]


case class ObsTumorMorphology
(
  id: String,
  status: Observation.Status.Value,
  subject: LogicalReference[Patient],
  specimen: LogicalReference[Specimen],
  valueCodeableConcept: CodeableConceptStatic[ICDO3M],
  note: Option[List[Note]],
)
extends ObsTumorMorphologyProfile

object ObsTumorMorphology
{

  import CodingSystems._

  implicit val profiles =
    Meta.Profiles[ObsTumorMorphology]("http://bwhc.de/mtb/observation-tumor-morphology")

  implicit val code =
    Code[ObsTumorMorphology](LOINC("59847-4", Some("Tumor Morphology")))

  implicit val format = Json.format[ObsTumorMorphology]
  
}



//-----------------------------------------------------------------------------
// HistologyReport
//-----------------------------------------------------------------------------


trait HistologyReportProfile
//extends DiagnosticReport
extends DiagnosticReportSC
   with DiagnosticReport.identifierNel
   with DiagnosticReport.subject[Patient,Required]
   with DiagnosticReport.specimenNel[TumorSpecimenProfile]
   with DiagnosticReport.issued[LocalDate,Optional]
   with DiagnosticReport.result[Observation,Required]
   with DiagnosticReport.contained[
     ContainedResources {
       val tumorMorphology: Option[ObsTumorMorphologyProfile]
       val tumorCellContent: Option[ObsTumorCellContentProfile]
     }
   ]


final case class HistologyReport
(
  identifier: NonEmptyList[Identifier],
  issued: Option[LocalDate],
  status: DiagnosticReport.Status.Value,
  subject: LogicalReference[MTBPatient],
  specimen: NonEmptyList[LogicalReference[TumorSpecimen]],
  result: List[LiteralReference[Observation]],
  contained: HistologyReport.Results
)
extends HistologyReportProfile


object HistologyReport
{

  implicit val profile =
   Meta.Profiles[HistologyReport]("http://bwhc.de/mtb/histology-report")

  implicit val code =
    Code[HistologyReport](LOINC("TODO: LOINC Code Histology Report",Some("Histology Report")))

  final case class Results
  (
    tumorMorphology: Option[ObsTumorMorphology],
    tumorCellContent: Option[ObsTumorCellContent]
  )
  extends ContainedResources


  import json.contained._

  implicit val format = Json.format[HistologyReport]

}
