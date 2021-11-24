package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.MedicationStatement._
import org.hl7.fhir.r4.Medication._
import org.hl7.fhir.r4.Patient._

import org.hl7.fhir.r4.json._

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.GuidelineTherapy


final case class TherapyLine(value: PositiveInt) extends SimpleExtension[PositiveInt]
object TherapyLine
{
  implicit val url    = Extension.Url[TherapyLine]("therapy-line")
  implicit val format = json.extensions.format(TherapyLine(_))
}


sealed trait GuidelineTherapyProfile
extends MedicationStatement
   with MedicationStatement.identifierNel
   with MedicationStatement.extension[TherapyLine,Optional]
   with MedicationStatement.contained[ContainedResources { val medication: MTBMedicationProfile }]
   with MedicationStatement.subject[Patient,Required]
   with MedicationStatement.reasonReferenceNel
   with MedicationStatement.medicationReference[MTBMedicationProfile]

trait LastGuidelineTherapyProfile
extends GuidelineTherapyProfile
   with MedicationStatement.effectivePeriod[OpenEndPeriod[LocalDate],Optional]
   with MedicationStatement.statusReason[
     CodeableConcept
       with CodeableConcept.codingNel[CodingStatic[GuidelineTherapy.StopReason.Value]],
     Optional
   ]


final case class ContainedMedication(
  medication: MTBMedication 
)
extends ContainedResources

final case class PreviousGuidelineTherapy
(
  identifier: NonEmptyList[Identifier],
  extension: Option[List[TherapyLine]],
  contained: ContainedMedication,
  status: MedicationStatement.Status.Value,
  subject: LogicalReference[MTBPatient],
  reasonReference: NonEmptyList[LogicalReference[Condition]],
  medicationReference: LiteralReference[MTBMedication]
)
extends GuidelineTherapyProfile


final case class LastGuidelineTherapy
(
  identifier: NonEmptyList[Identifier],
  extension: Option[List[TherapyLine]],
  contained: ContainedMedication,
  status: MedicationStatement.Status.Value,
  statusReason: Optional[List[CodeableConceptStatic[GuidelineTherapy.StopReason.Value]]],
  subject: LogicalReference[MTBPatient],
  reasonReference: NonEmptyList[LogicalReference[Condition]],
  effectivePeriod: Option[OpenEndPeriod[LocalDate]],
  medicationReference: LiteralReference[MTBMedication]
)
extends LastGuidelineTherapyProfile


object PreviousGuidelineTherapy
{

  implicit val profiles =
    Meta.Profiles[PreviousGuidelineTherapy]("http://bwhc.de/mtb/previous-guideline-therapy")
  
  import json.contained._
  
  implicit val format = Json.format[PreviousGuidelineTherapy]
  
}

object LastGuidelineTherapy
{

  implicit val profiles =
    Meta.Profiles[LastGuidelineTherapy]("http://bwhc.de/mtb/last-guideline-therapy")


  implicit val stopReasonSystem =
    CodingSystem[GuidelineTherapy.StopReason.Value]("http://bwhc.de/mtb/guideline-therapy/stopreason")

    
  import json.contained._

  implicit val format = Json.format[LastGuidelineTherapy]
  
}
