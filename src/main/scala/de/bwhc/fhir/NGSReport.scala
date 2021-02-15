package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Observation._
import org.hl7.fhir.r4.DiagnosticReport._
import org.hl7.fhir.r4.json._

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.TumorCellContent


//-----------------------------------------------------------------------------
// Tumor Content
//-----------------------------------------------------------------------------

abstract class ObsTumorCellContentProfile
extends Observation
   with Observation.id[Required]
   with Observation.subject[Patient,Required]
   with Observation.specimen[Required]
   with Observation.method[
     CodeableConcept
       with CodeableConcept.codingNel[Coding[TumorCellContent.Method.Value]],
     Required
   ]
   with Observation.valueQuantity[Quantity,Required]


case class ObsTumorCellContent
(
  id: String,
  status: Observation.Status.Value,
  subject: LogicalReference[MTBPatient],
  specimen: LogicalReference[TumorSpecimen],
  method: BasicCodeableConcept[TumorCellContent.Method.Value],
  valueQuantity: SimpleQuantity
)
extends ObsTumorCellContentProfile

object ObsTumorCellContent
{

  implicit val profiles =
    Meta.Profiles[ObsTumorCellContent]("http://observation-tumor-content")

//  implicit val code = LOINC.Code[ObsTumorCellContent]("TODO: LOINC tumor-content", Some("Tumor Content"))
  implicit val code =
    Code[ObsTumorCellContent](LOINC("TODO: LOINC tumor-content", Some("Tumor Content")))

  implicit val tcMethodSystem =
    Coding.System[TumorCellContent.Method.Value]("tumor-content-method")
    
  implicit val format = Json.format[ObsTumorCellContent]
  
}


//-----------------------------------------------------------------------------
// Tumor Mutational Burden (TMB)
//-----------------------------------------------------------------------------

abstract class ObsTMBProfile
extends Observation
   with Observation.id[Required]
   with Observation.subject[Patient,Required]
   with Observation.specimen[Required]
   with Observation.valueQuantity[Quantity,Required]


case class ObsTMB
(
  id: String,
  status: Observation.Status.Value,
  subject: LogicalReference[MTBPatient],
  specimen: LogicalReference[TumorSpecimen],
  valueQuantity: SimpleQuantity
)
extends ObsTMBProfile

object ObsTMB
{
  implicit val profiles =
    Meta.Profiles[ObsTMB]("http://observation-tumor-mutational-burden")

  implicit val code = Code[ObsTMB](LOINC("TODO: LOINC TMB", Some("Tumor Mutational Burden")))
//  implicit val code = LOINC.Code[ObsTMB]("TODO: LOINC TMB", Some("Tumor Mutational Burden"))

  implicit val format = Json.format[ObsTMB]
}


//-----------------------------------------------------------------------------
// Micro-Satellite Instabilities (MSI)
//-----------------------------------------------------------------------------

abstract class ObsMSIProfile
extends Observation
   with Observation.id[Required]
   with Observation.subject[Patient,Required]
   with Observation.specimen[Required]
   with Observation.valueQuantity[Quantity,Required]


case class ObsMSI
(
  id: String,
  status: Observation.Status.Value,
  subject: LogicalReference[MTBPatient],
  specimen: LogicalReference[TumorSpecimen],
  valueQuantity: SimpleQuantity
)
extends ObsMSIProfile

object ObsMSI
{
  implicit val profiles = Meta.Profiles[ObsMSI]("http://observation-msi")

//  implicit val code = LOINC.Code[ObsMSI]("TODO: LOINC MSI", Some("Micro-Satellite Instabilities (MSI)"))
  implicit val code =
    Code[ObsMSI](LOINC("TODO: LOINC MSI", Some("Micro-Satellite Instabilities (MSI)")))

  implicit val format = Json.format[ObsMSI]
}


//-----------------------------------------------------------------------------
// BRCAness
//-----------------------------------------------------------------------------

abstract class ObsBRCAnessProfile
extends Observation
   with Observation.id[Required]
   with Observation.subject[Patient,Required]
   with Observation.specimen[Required]
   with Observation.valueQuantity[Quantity,Required]


case class ObsBRCAness
(
  id: String,
  status: Observation.Status.Value,
  subject: LogicalReference[MTBPatient],
  specimen: LogicalReference[TumorSpecimen],
  valueQuantity: SimpleQuantity
)
extends ObsBRCAnessProfile

object ObsBRCAness
{
  implicit val profiles = Meta.Profiles[ObsBRCAness]("http://observation-brcaness")

  implicit val code =
    Code[ObsBRCAness](LOINC("TODO: LOINC BRCAness", Some("BRCAness")))
//  implicit val code = LOINC.Code[ObsBRCAness]("TODO: LOINC BRCAness", Some("BRCAness"))

  implicit val format = Json.format[ObsBRCAness]
}



//-----------------------------------------------------------------------------
// NGSReport Profile
//-----------------------------------------------------------------------------

trait SomaticNGSReportProfile
extends DiagnosticReport
   with DiagnosticReport.identifierNel
   with DiagnosticReport.subject[Patient,Required]
   with DiagnosticReport.specimenNel[TumorSpecimenProfile]
   with DiagnosticReport.issued[LocalDate,Required]
   with DiagnosticReport.resultNel[Observation]
//   with DiagnosticReport.contained[SomaticNGSReportProfile.Results]
   with DiagnosticReport.contained[
     Product5[
       List[ObsTumorCellContentProfile],
       ObsTMBProfile,
       ObsMSIProfile,
       ObsBRCAnessProfile,
       List[SimpleVariantProfile]
     ]
   ]


/*
object SomaticNGSReportProfile
{
  trait Results
  {
    this: Product =>

    val tumorContent:   List[ObsTumorCellContentProfile]
    val tmb:            ObsTMBProfile
    val msi:            ObsMSIProfile
    val brcaness:       ObsBRCAnessProfile
    val simpleVariants: List[SimpleVariantProfile]
  }
}
*/



final case class SomaticNGSReport
(
  identifier: NonEmptyList[Identifier],
  issued: LocalDate,
  status: DiagnosticReport.Status.Value,
  subject: LogicalReference[MTBPatient],
  specimen: NonEmptyList[LogicalReference[TumorSpecimen]],
  result: NonEmptyList[LiteralReference[Observation]],
  contained: (
    List[ObsTumorCellContent],
    ObsTMB,
    ObsMSI,
    ObsBRCAness,
    List[SimpleVariant]
  )

)
extends SomaticNGSReportProfile


object SomaticNGSReport
{

  implicit val profile =
   Meta.Profiles[SomaticNGSReport]("http://bwhc.de/mtb/somatic-ngs-report")

//  implicit val code = LOINC.Code[SomaticNGSReport]("TODO: LOINC NGS Report",Some("Somatic NGS Report"))
  implicit val code =
    Code[SomaticNGSReport](LOINC("TODO: LOINC NGS Report",Some("Somatic NGS Report")))

  import json.contained._

  implicit val format = Json.format[SomaticNGSReport]

}


