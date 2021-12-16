package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.CarePlan._
import org.hl7.fhir.r4.MedicationRequest._
import org.hl7.fhir.r4.ServiceRequest._

import play.api.libs.json.{Json, Format}

import de.bwhc.mtb.data.entry.dtos.LevelOfEvidence



//-----------------------------------------------------------------------------
// Therapy Recommendation
//-----------------------------------------------------------------------------

final case class LoE
(
  grade: LoE.Grade,
  addendums: Set[LoE.Addendum]
)
extends Extension


object LoE
{

  import json._
  import json.extensions._

  implicit val loeGrading =
    CodingSystem[LevelOfEvidence.Grading.Value]("mtb-level-of-evidence:grading")

  implicit val loeAdd =
    CodingSystem[LevelOfEvidence.Addendum.Value]("mtb-level-of-evidence:addendum")


  final case class Grade(
    value: CodingStatic[LevelOfEvidence.Grading.Value]
  )
  extends SimpleExtension[CodingStatic[LevelOfEvidence.Grading.Value]]

  final case class Addendum(
    value: CodingStatic[LevelOfEvidence.Addendum.Value]
  )
  extends SimpleExtension[CodingStatic[LevelOfEvidence.Addendum.Value]]

  object Grade
  {
    implicit val url    = Extension.Url[Grade]("http://bwhc.de/mtb/therapy-recommendation/level-of-evidence/grading")
    implicit val format = json.extensions.format(Grade(_))
  }

  object Addendum
  {
    implicit val url    = Extension.Url[Addendum]("http://bwhc.de/mtb/therapy-recommendation/level-of-evidence/addendum")
    implicit val format = json.extensions.format(Addendum(_))
  }

  implicit val url       = Extension.Url[LoE]("http://bwhc.de/mtb/therapy-recommendation/level-of-evidence")
  implicit val formatLoE = json.extensions.format[LoE]

}


trait TherapyRecommendationProfile
extends MedicationRequest
   with MedicationRequest.identifierNel
   with MedicationRequest.extension[LoE,Optional]
   with MedicationRequest.subject[Patient]
   with MedicationRequest.reasonReferenceNel
   with MedicationRequest.contained[
     ContainedResources {
       val medication: MTBMedicationProfile
     }
   ]
   with MedicationRequest.authoredOn[LocalDate,Optional]
   with MedicationRequest.priority[Optional]
   with MedicationRequest.medicationReference[Medication]
   with MedicationRequest.supportingInformation[DomainResource,Optional]



final case class TherapyRecommendation
(
  identifier: NonEmptyList[Identifier],
  extension: Option[List[LoE]],
  contained: ContainedMedication,  
  priority: Option[MedicationRequest.Priority.Value],
  status: MedicationRequest.Status.Value,
  intent: MedicationRequest.Intent.Value,
  authoredOn: Option[LocalDate],
  subject: LogicalReference[MTBPatient],
  reasonReference: NonEmptyList[LogicalReference[Condition]],
  medicationReference: LiteralReference[Medication],
  supportingInformation: Option[List[LogicalReference[DomainResource]]]
) extends TherapyRecommendationProfile

object TherapyRecommendation
{

  import org.hl7.fhir.r4.json._
  import org.hl7.fhir.r4.json.contained._

  implicit val profiles =
    Meta.Profiles[TherapyRecommendation]("http://bwhc.de/mtb/therapy-recommendation")
    
  implicit val format = Json.format[TherapyRecommendation]
}



//-----------------------------------------------------------------------------
// Genetic Counselling Recommendation
//-----------------------------------------------------------------------------

trait CounsellingRequestProfile
extends ServiceRequest
   with ServiceRequest.identifierNel
   with ServiceRequest.subject[Patient]
   with MedicationRequest.authoredOn[LocalDate,Optional]
//   with ServiceRequest.categoryNel[
//     CodeableConcept with CodeableConcept.codingNel[Coding[SNOMEDCT]]
//   ]
//   with ServiceRequest.code[
//     CodeableConcept with CodeableConcept.codingNel[Coding[SNOMEDCT]],
//       Required
//   ]
   with ServiceRequest.noteNel[Annotation]


final case class CounsellingRequest
(
  identifier: NonEmptyList[Identifier],
  status: ServiceRequest.Status.Value,
  intent: ServiceRequest.Intent.Value,
  subject: LogicalReference[Patient],
  authoredOn: Option[LocalDate],
//  category: NonEmptyList[BasicCodeableConcept[SNOMEDCT]],
//  code: BasicCodeableConcept[SNOMEDCT]
  note: NonEmptyList[Note]
)
extends CounsellingRequestProfile

object CounsellingRequest
{

  implicit val profiles =
    Meta.Profiles[CounsellingRequest]("http://bwhc.de/mtb/genetic-counselling-request")
  
  import org.hl7.fhir.r4.json._

  implicit val format = Json.format[CounsellingRequest]

}


//-----------------------------------------------------------------------------
// Re-biopsy Request
//-----------------------------------------------------------------------------

trait RebiopsyRequestProfile
extends ServiceRequest
   with ServiceRequest.identifierNel
   with ServiceRequest.subject[Patient]
   with MedicationRequest.authoredOn[LocalDate,Optional]
//   with ServiceRequest.categoryNel[
//     CodeableConcept with CodeableConcept.codingNel[Coding[SNOMEDCT]]
//   ]
//   with ServiceRequest.code[
//     CodeableConcept with CodeableConcept.codingNel[Coding[SNOMEDCT]],
//       Required
//   ]
   with ServiceRequest.specimenNel


final case class RebiopsyRequest
(
  identifier: NonEmptyList[Identifier],
  status: ServiceRequest.Status.Value,
  intent: ServiceRequest.Intent.Value,
  subject: LogicalReference[Patient],
  authoredOn: Option[LocalDate],
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


//-----------------------------------------------------------------------------
// Histology Reeavaluation Request
//-----------------------------------------------------------------------------

trait HistologyReevaluationRequestProfile
extends ServiceRequest
   with ServiceRequest.identifierNel
   with ServiceRequest.subject[Patient]
   with MedicationRequest.authoredOn[LocalDate,Optional]
//   with ServiceRequest.categoryNel[
//     CodeableConcept with CodeableConcept.codingNel[Coding[SNOMEDCT]]
//   ]
//   with ServiceRequest.code[
//     CodeableConcept with CodeableConcept.codingNel[Coding[SNOMEDCT]],
//       Required
//   ]
   with ServiceRequest.specimenNel


final case class HistologyReevaluationRequest
(
  identifier: NonEmptyList[Identifier],
  status: ServiceRequest.Status.Value,
  intent: ServiceRequest.Intent.Value,
  subject: LogicalReference[Patient],
  authoredOn: Option[LocalDate],
  specimen: NonEmptyList[LogicalReference[TumorSpecimen]],
)
extends HistologyReevaluationRequestProfile

object HistologyReevaluationRequest
{

  implicit val profiles =
    Meta.Profiles[HistologyReevaluationRequest]("http://bwhc.de/mtb/histology-reevaluation-request")
  
  import org.hl7.fhir.r4.json._

  implicit val format = Json.format[HistologyReevaluationRequest]

}


//-----------------------------------------------------------------------------
// MTB CarePlan
//-----------------------------------------------------------------------------

trait MTBCarePlanProfile
extends CarePlan
   with CarePlan.identifierNel
   with CarePlan.subject[Patient]
   with CarePlan.addressesNel
   with CarePlan.created[LocalDate,Optional]
   with CarePlan.description[Optional]
   with CarePlan.activities[
     CarePlan.ActivitySet {
       val noTarget:
         Option[
           CarePlan.ActivityElement
             with CarePlan.Activity.detail[
               CarePlan.Activity.DetailElement
                 with CarePlan.Activity.Detail.statusReason[
                   CodeableConceptDynamic, Required
                 ]
             ]
         ]

       val requests:
         List[
           CarePlan.ActivityElement
             with CarePlan.Activity.reference[DomainResource with Request]
         ]
  
       val studyInclusionRequests: 
         List[
           CarePlan.ActivityElement
             with CarePlan.Activity.detail[
               CarePlan.Activity.DetailElement
                 with CarePlan.Activity.Detail.code[
                   CodeableConceptDynamic,
                   Required
                 ]
                 with CarePlan.Activity.Detail.extensionNel[
                   SimpleExtension[Reference[ResearchStudy]]
                 ]
                 with CarePlan.Activity.Detail.reasonReferenceNel
             ]
         ]
     },
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
  activity: MTBCarePlan.Activities 
)
extends MTBCarePlanProfile


object MTBCarePlan
{

  implicit val profiles =
    Meta.Profiles[MTBCarePlan]("http://bwhc.de/mtb/careplan")


  final case class RequestReference
  (
    reference: LogicalReference[DomainResource with Request]
  )
  extends CarePlan.ActivityElement
     with CarePlan.Activity.reference[DomainResource with Request]
  
  implicit val formatRequestReference = Json.format[RequestReference]


  final case class Activity[D <: CarePlan.Activity.DetailElement]
  (
    detail: D
  ) 
  extends CarePlan.ActivityElement
     with CarePlan.Activity.detail[D]

  implicit def formatActivity[D <: CarePlan.Activity.DetailElement: Format] =
    Json.format[Activity[D]]


  final case class NoTarget
  (
    status: CarePlan.Activity.Detail.Status.Value,
    statusReason: CodeableConceptDynamic
  )
  extends CarePlan.Activity.DetailElement
     with CarePlan.Activity.Detail.statusReason[CodeableConceptDynamic,Required]
  
  implicit val formatNoTarget = Json.format[NoTarget]
    
    
  final case class NCTStudyReference
  (
    value: LogicalReference[ResearchStudy]
  )
  extends SimpleExtension[LogicalReference[ResearchStudy]]
  
  object NCTStudyReference
  {  
    import json.extensions._
  
    implicit val url =
      Extension.Url[NCTStudyReference]("http://bwhc.de/mtb/study-inclusion-request/nct-number")

    implicit val format =
      json.extensions.format(NCTStudyReference(_))
  }
  
  
  final case class StudyInclusionRequest
  (
    status: CarePlan.Activity.Detail.Status.Value,
    code: CodeableConceptDynamic,
    extension: NonEmptyList[NCTStudyReference],
    reasonReference: NonEmptyList[LogicalReference[Condition]]
  )
  extends CarePlan.Activity.DetailElement
     with CarePlan.Activity.Detail.code[CodeableConceptDynamic,Required]
     with CarePlan.Activity.Detail.extensionNel[SimpleExtension[Reference[ResearchStudy]]]
     with CarePlan.Activity.Detail.reasonReferenceNel

  import org.hl7.fhir.r4.json.formatNel

  implicit val formatStudyInclusionRequest = Json.format[StudyInclusionRequest]

    

  final case class Activities
  (
    requests: List[RequestReference],
    noTarget: Option[Activity[NoTarget]],
    studyInclusionRequests: List[Activity[StudyInclusionRequest]]
  ) extends CarePlan.ActivitySet



  import org.hl7.fhir.r4.json._
  import org.hl7.fhir.r4.json.backboneElements._


  implicit val format = Json.format[MTBCarePlan]
  
}
