package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.CarePlan._
import org.hl7.fhir.r4.MedicationRequest._
import org.hl7.fhir.r4.ServiceRequest._

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.LevelOfEvidence


final case class LoE(
  grade: LoE.Grade,
  addendums: Set[LoE.Addendum]
)
extends Extension


object LoE
{

  import json._
  import json.extensions._

  implicit val loeGrading =
    Coding.System[LevelOfEvidence.Grading.Value]("mtb-level-of-evidence:grading")

  implicit val loeAdd =
    Coding.System[LevelOfEvidence.Addendum.Value]("mtb-level-of-evidence:addendum")


  final case class Grade(
    value: BasicCoding[LevelOfEvidence.Grading.Value]
  )
  extends SimpleExtension[BasicCoding[LevelOfEvidence.Grading.Value]]

  final case class Addendum(
    value: BasicCoding[LevelOfEvidence.Addendum.Value]
  )
  extends SimpleExtension[BasicCoding[LevelOfEvidence.Addendum.Value]]

  object Grade
  {
    implicit val url    = Extension.Url[Grade]("mtb-level-of-evidence:grading")
    implicit val format = json.extensions.format(Grade(_))
  }

  object Addendum
  {
    implicit val url    = Extension.Url[Addendum]("mtb-level-of-evidence:addendum")
    implicit val format = json.extensions.format(Addendum(_))
  }

  implicit val url       = Extension.Url[LoE]("mtb-level-of-evidence")
  implicit val formatLoE = json.extensions.format[LoE]

}


trait TherapyRecommendationProfile
extends MedicationRequest
   with MedicationRequest.identifierNel
   with MedicationRequest.extensions[Product1[LoE],Optional]
   with MedicationRequest.subject[Patient]
   with MedicationRequest.reasonReferenceNel
   with MedicationRequest.contained[Product1[MTBMedicationProfile]]
   with MedicationRequest.authoredOn[LocalDate,Optional]
   with MedicationRequest.priority[Optional]
   with MedicationRequest.medicationReference[Medication]
   with MedicationRequest.supportingInformation[SomaticVariantProfile,Optional]



final case class TherapyRecommendation
(
  identifier: NonEmptyList[Identifier],
  extension: Option[Tuple1[LoE]],
  contained: Tuple1[MTBMedication],
  priority: Option[MedicationRequest.Priority.Value],
  status: MedicationRequest.Status.Value,
  intent: MedicationRequest.Intent.Value,
  authoredOn: Option[LocalDate],
  subject: LogicalReference[MTBPatient],
  reasonReference: NonEmptyList[LogicalReference[Condition]],
  medicationReference: LiteralReference[Medication],
  supportingInformation: Option[List[LogicalReference[SomaticVariantProfile]]]
//  supportingInformation: NonEmptyList[Reference[SomaticVariantProfile]]
) extends TherapyRecommendationProfile

object TherapyRecommendation
{

  import org.hl7.fhir.r4.json._
  import org.hl7.fhir.r4.json.contained._

  implicit val profiles =
    Meta.Profiles[TherapyRecommendation]("http://bwhc.de/mtb/therapy-recommendation")
    
  implicit val format = Json.format[TherapyRecommendation]
}



trait CounsellingRequestProfile
extends ServiceRequest
   with ServiceRequest.identifierNel
   with ServiceRequest.subject[Patient]
//   with ServiceRequest.categoryNel[
//     CodeableConcept with CodeableConcept.codingNel[Coding[SNOMEDCT]]
//   ]
//   with ServiceRequest.code[
//     CodeableConcept with CodeableConcept.codingNel[Coding[SNOMEDCT]],
//       Required
//   ]


final case class CounsellingRequest
(
  identifier: NonEmptyList[Identifier],
  status: ServiceRequest.Status.Value,
  intent: ServiceRequest.Intent.Value,
  subject: LogicalReference[Patient],
//  category: NonEmptyList[BasicCodeableConcept[SNOMEDCT]],
//  code: BasicCodeableConcept[SNOMEDCT]
)
extends CounsellingRequestProfile

object CounsellingRequest
{

  implicit val profiles =
    Meta.Profiles[CounsellingRequest]("http://bwhc.de/mtb/genetic-counselling-request")
  
  import org.hl7.fhir.r4.json._

  implicit val format = Json.format[CounsellingRequest]

}


trait RebiopsyRequestProfile
extends ServiceRequest
   with ServiceRequest.identifierNel
   with ServiceRequest.subject[Patient]
//   with ServiceRequest.categoryNel[
//     CodeableConcept with CodeableConcept.codingNel[Coding[SNOMEDCT]]
//   ]
//   with ServiceRequest.code[
//     CodeableConcept with CodeableConcept.codingNel[Coding[SNOMEDCT]],
//       Required
//   ]
   with ServiceRequest.specimenNel[TumorSpecimen]


final case class RebiopsyRequest
(
  identifier: NonEmptyList[Identifier],
  status: ServiceRequest.Status.Value,
  intent: ServiceRequest.Intent.Value,
  subject: LogicalReference[Patient],
//  category: NonEmptyList[BasicCodeableConcept[SNOMEDCT]],
//  code: BasicCodeableConcept[SNOMEDCT],
  specimen: NonEmptyList[LogicalReference[TumorSpecimen]],
)
extends RebiopsyRequestProfile

object RebiopsyRequest
{

  implicit val profiles =
    Meta.Profiles[RebiopsyRequest]("http://bwhc.de/mtb/rebiopsy-request")
  
  import org.hl7.fhir.r4.json._

  implicit val format = Json.format[RebiopsyRequest]

}



trait MTBCarePlanProfile
extends CarePlan
   with CarePlan.identifierNel
   with CarePlan.subject[Patient]
   with CarePlan.addressesNel
   with CarePlan.created[LocalDate,Optional]
   with CarePlan.description[Optional]
   with CarePlan.activity[
     CarePlan.ActivityElement
       with CarePlan.Activity.reference[TherapyRecommendationProfile],
     Required
   ]


final case class MTBCarePlan
(
  identifier: NonEmptyList[Identifier],
  status: CarePlan.Status.Value,
  intent: CarePlan.Intent.Value,
  created: Option[LocalDate],
  subject: LogicalReference[MTBPatient],
  addresses: NonEmptyList[LogicalReference[Diagnosis]],
  description: Option[String],
  activity: List[MTBCarePlan.Activity] 
)
extends MTBCarePlanProfile


object MTBCarePlan
{

  implicit val profiles =
    Meta.Profiles[MTBCarePlan]("http://bwhc.de/mtb/careplan")
  
  
  case class Activity
  (
    reference: LogicalReference[TherapyRecommendation]
  )
  extends CarePlan.ActivityElement
     with CarePlan.Activity.reference[TherapyRecommendation]


  import org.hl7.fhir.r4.json._
  import org.hl7.fhir.r4.json.backboneElements._


  implicit val formatActivity = Json.format[Activity]

  implicit val format = Json.format[MTBCarePlan]
  
}
