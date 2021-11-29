package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList


import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Observation._
import org.hl7.fhir.r4.DiagnosticReport._
import org.hl7.fhir.r4.json._

import play.api.libs.json.Json


import de.bwhc.mtb.data.entry.dtos.MolecularPathologyFinding


//-----------------------------------------------------------------------------
// MolecularPathologyReport
//-----------------------------------------------------------------------------

trait MolecularPathologyReportProfile
extends DiagnosticReportSC
   with DiagnosticReport.identifierNel
   with DiagnosticReport.subject[Patient,Required]
   with DiagnosticReport.specimenNel[TumorSpecimenProfile]
   with DiagnosticReport.issued[LocalDate,Optional]
   with DiagnosticReport.performer[Organization,Optional]
   with DiagnosticReport.conclusion[Required]


final case class MolecularPathologyReport
(
  identifier: NonEmptyList[Identifier],
  issued: Option[LocalDate],
  status: DiagnosticReport.Status.Value,
  subject: LogicalReference[MTBPatient],
  specimen: NonEmptyList[LogicalReference[TumorSpecimen]],
  performer: Option[List[LogicalReference[Organization]]],
  conclusion: String
)
extends MolecularPathologyReportProfile


object MolecularPathologyReport
{

  implicit val profile =
   Meta.Profiles[MolecularPathologyReport]("http://bwhc.de/mtb/molecular-pathology-report")

  implicit val code =
    Code[MolecularPathologyReport](LOINC("TODO: LOINC Code MolecularPathology Report",Some("MolecularPathology Report")))


  implicit val format = Json.format[MolecularPathologyReport]

}
