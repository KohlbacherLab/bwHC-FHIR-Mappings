package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Condition._
import org.hl7.fhir.r4.Observation._
import org.hl7.fhir.r4.Patient._

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos
import de.bwhc.mtb.data.entry.dtos.{
  ICD10GM, ICDO3T, WHOGrade
}

import CodingSystems._


object DiagnosisProfile
{

  final case class GuidelineTreatmentStatus
  (
    value: CodingStatic[dtos.GuidelineTreatmentStatus.Value]
  )
  extends SimpleExtension[CodingStatic[dtos.GuidelineTreatmentStatus.Value]]

  object GuidelineTreatmentStatus
  {
    implicit val url    = Extension.Url[GuidelineTreatmentStatus]("http://bwhc.de/mtb/diagnosis/guideline-treatment-status")
    implicit val format = json.extensions.format(GuidelineTreatmentStatus(_))
  }

}


trait DiagnosisProfile
extends Condition
   with Condition.identifierNel
   with Condition.recordedDate[LocalDate,Optional]
   with Condition.subject[Patient,Required]
   with Condition.code[
     CodeableConcept with CodeableConcept.codingNel[CodingStatic[ICD10GM]],
     Optional
   ]
   with Condition.bodySite[
     CodeableConcept with CodeableConcept.codingNel[CodingStatic[ICDO3T]],
     Optional
   ]
   with Condition.stages[
     Product {
       val tumorStages: List[
         Condition.StageElement
           with Condition.Stage.summary[
             CodeableConcept with CodeableConcept.codingNel[CodingStatic[dtos.Diagnosis.Status.Value]]
           ]
           with Condition.Stage.extension[SimpleExtension[LocalDate],Optional]
       ]

       val whoGrade: Option[
         Condition.StageElement
         with Condition.Stage.summary[
           CodeableConcept with CodeableConcept.codingNel[CodingStatic[WHOGrade.Value]]
         ]
         with Condition.Stage.extension[SimpleExtension[LocalDate],Optional]
       ]
     },
     Required
   ]
   with Condition.evidence[
     Condition.EvidenceElement with Condition.Evidence.detail[ObsHistologyProfile],
     Optional
   ]
   with Condition.extension[DiagnosisProfile.GuidelineTreatmentStatus,Optional]



case class Diagnosis
(
  identifier: NonEmptyList[Identifier],
  subject: LogicalReference[MTBPatient],
  recordedDate: Option[LocalDate],
  code: Option[CodeableConceptStatic[ICD10GM]],
  bodySite: Option[List[CodeableConceptStatic[ICDO3T]]],
  stage: Diagnosis.Stages,
  evidence: Option[List[Diagnosis.HistologyEvidence]],
  extension: Option[List[DiagnosisProfile.GuidelineTreatmentStatus]]
)
extends DiagnosisProfile



object Diagnosis
{

  import org.hl7.fhir.r4.json._
  import org.hl7.fhir.r4.json.backboneElements._


  object Stage
  {
    final case class Date(value: LocalDate) extends SimpleExtension[LocalDate]

    implicit val url    = Extension.Url[Date]("http://bwhc.de/mtb/diagnosis/tumor-stage-date")
    implicit val format = json.extensions.format(Date)
  }

  case class Stage[T](
    extension: Option[List[Stage.Date]],
    summary: CodeableConceptStatic[T]
  )
  extends Condition.StageElement
     with Condition.Stage.summary[CodeableConceptStatic[T]]
     with Condition.Stage.extension[Stage.Date,Optional]

  final case class Stages(
    tumorStages: List[Diagnosis.Stage[dtos.Diagnosis.Status.Value]],
    whoGrade: Option[Diagnosis.Stage[WHOGrade.Value]]
  )


  case class HistologyEvidence(
    detail: NonEmptyList[LogicalReference[ObsHistology]]
  )
  extends Condition.EvidenceElement
     with Condition.Evidence.detail[ObsHistology]
 

  implicit val profiles =
    Meta.Profiles[Diagnosis]("http://bwhc.de/mtb/diagnosis")
    
  implicit val formatEvidence = Json.format[HistologyEvidence]

  implicit def formatStage[T: CodingSystem] = Json.format[Stage[T]]

  implicit val formatDiagnosis = Json.format[Diagnosis]
  
}
