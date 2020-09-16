package de.bwhc.fhir


import java.time.LocalDate
//import java.time.Instant

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Observation._
import org.hl7.fhir.r4.DiagnosticReport._
import org.hl7.fhir.r4.json._

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.TumorContent


//-----------------------------------------------------------------------------
// Tumor Content
//-----------------------------------------------------------------------------

abstract class ObsTumorContentProfile
extends Observation
   with Observation.id[Required]
   with Observation.subject[Patient,Required]
   with Observation.specimen[Required]
   with Observation.method[
     CodeableConcept
     with CodeableConcept.codingNel[Coding[TumorContent.Method.Value]],
     Required
   ]
   with Observation.valueQuantity[Quantity,Required]


case class ObsTumorContent
(
  id: String,
  status: Observation.Status.Value,
  subject: Reference[MTBPatient],
  specimen: Reference[TumorSpecimen],
  method: BasicCodeableConcept[TumorContent.Method.Value],
  valueQuantity: SimpleQuantity
)
extends ObsTumorContentProfile

object ObsTumorContent
{

  implicit val profiles =
    Meta.Profiles[ObsTumorContent]("http://observation-tumor-content")

  implicit val code = LOINC.Code[ObsTumorContent]("TODO: LOINC tumor-content", Some("Tumor Content"))

  implicit val tcMethodSystem =
    Coding.System[TumorContent.Method.Value]("tumor-content-method")
    
  implicit val format = Json.format[ObsTumorContent]
  
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
  subject: Reference[MTBPatient],
  specimen: Reference[TumorSpecimen],
  valueQuantity: SimpleQuantity
)
extends ObsTMBProfile

object ObsTMB
{
  implicit val profiles =
    Meta.Profiles[ObsTMB]("http://observation-tumor-mutational-burden")

  implicit val code = LOINC.Code[ObsTMB]("TODO: LOINC TMB", Some("Tumor Mutational Burden"))

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
  subject: Reference[MTBPatient],
  specimen: Reference[TumorSpecimen],
  valueQuantity: SimpleQuantity
)
extends ObsMSIProfile

object ObsMSI
{
  implicit val profiles = Meta.Profiles[ObsMSI]("http://observation-msi")

  implicit val code = LOINC.Code[ObsMSI]("TODO: LOINC MSI", Some("Micro-Satellite Instabilities (MSI)"))

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
  subject: Reference[MTBPatient],
  specimen: Reference[TumorSpecimen],
  valueQuantity: SimpleQuantity
)
extends ObsBRCAnessProfile

object ObsBRCAness
{
  implicit val profiles = Meta.Profiles[ObsBRCAness]("http://observation-brcaness")

  implicit val code = LOINC.Code[ObsBRCAness]("TODO: LOINC BRCAness", Some("BRCAness"))

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
       List[ObsTumorContentProfile],
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

    val tumorContent:   List[ObsTumorContentProfile]
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
  subject: Reference[MTBPatient],
  specimen: NonEmptyList[Reference[TumorSpecimen]],
  result: NonEmptyList[Reference[Observation]],
  contained: (
    List[ObsTumorContent],
    ObsTMB,
    ObsMSI,
    ObsBRCAness,
    List[SimpleVariant]
  )

)
extends SomaticNGSReportProfile


object SomaticNGSReport
{

/*
  final case class Results
  (
    tumorContent:   List[ObsTumorContentProfile],
    tmb:            ObsTMBProfile,
    msi:            ObsMSIProfile,
    brcaness:       ObsBRCAnessProfile,
    simpleVariants: List[SimpleVariantProfile]
  )
  extends SomaticNGSReportProfile.Results
*/

  implicit val profile = Meta.Profiles[SomaticNGSReport]("http://bwhc-somatic-ngs-report")

  implicit val code = LOINC.Code[SomaticNGSReport]("TODO: LOINC NGS Report",Some("Somatic NGS Report"))

  import json.contained._

  implicit val format = Json.format[SomaticNGSReport]

}


