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
   with MedicationStatement.modifierExtensions[Product1[TherapyLine]]
   with MedicationStatement.contained[Product1[MTBMedicationProfile]]
   with MedicationStatement.subject[Patient,Required]
   with MedicationStatement.medicationReference[MTBMedicationProfile]

trait LastGuidelineTherapyProfile
extends GuidelineTherapyProfile
   with MedicationStatement.effectivePeriod[ClosedPeriod[LocalDate],Required]
   with MedicationStatement.statusReasonNel[
     CodeableConcept
     with CodeableConcept.codingNel[Coding[GuidelineTherapy.StopReason.Value]]
   ]


final case class PreviousGuidelineTherapy
(
  identifier: NonEmptyList[Identifier],
  modifierExtension: Tuple1[TherapyLine],
  contained: Tuple1[MTBMedication],
  status: MedicationStatement.Status.Value,
  subject: Reference[MTBPatient],
  medicationReference: Reference[MTBMedication]
) extends GuidelineTherapyProfile


final case class LastGuidelineTherapy
(
  identifier: NonEmptyList[Identifier],
  modifierExtension: Tuple1[TherapyLine],
  contained: Tuple1[MTBMedication],
  status: MedicationStatement.Status.Value,
  statusReason: NonEmptyList[BasicCodeableConcept[GuidelineTherapy.StopReason.Value]],
  subject: Reference[MTBPatient],
  effectivePeriod: ClosedPeriod[LocalDate],
  medicationReference: Reference[MTBMedication]
) extends LastGuidelineTherapyProfile


object PreviousGuidelineTherapy
{

  implicit val profiles =
    Meta.Profiles[PreviousGuidelineTherapy]("http://bwhc-mtb-previous-guideline-therapy")
  
  import json.contained._
  
  implicit val format = Json.format[PreviousGuidelineTherapy]
  
}

object LastGuidelineTherapy
{

  implicit val profiles =
    Meta.Profiles[LastGuidelineTherapy]("http://bwhc-mtb-last-guideline-therapy")


  implicit val stopReasonSystem =
    Coding.System[GuidelineTherapy.StopReason.Value]("bwhc-mtb-guidelinetherapy-stopreason")

    
  import json.contained._

  implicit val format = Json.format[LastGuidelineTherapy]
  
}
