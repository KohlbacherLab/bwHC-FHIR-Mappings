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


trait DiagnosisProfile
extends Condition
   with Condition.identifierNel
   with Condition.recordedDate[LocalDate,Optional]
   with Condition.subject[Patient,Required]
   with Condition.code[
     CodeableConcept with CodeableConcept.codingNel[Coding[ICD10GM]],
     Optional
   ]
   with Condition.bodySite[
     CodeableConcept with CodeableConcept.codingNel[Coding[ICDO3T]],
     Optional
   ]
   with Condition.stages[
     Product2[
       List[
         Condition.StageElement
           with Condition.Stage.summary[
             CodeableConcept with CodeableConcept.codingNel[Coding[dtos.Diagnosis.Status.Value]]
           ]
           with Condition.Stage.extensions[Product1[SimpleExtension[LocalDate]],Optional]
       ],
       Option[
         Condition.StageElement
         with Condition.Stage.summary[
           CodeableConcept with CodeableConcept.codingNel[Coding[WHOGrade.Value]]
         ]
         with Condition.Stage.extensions[Product1[SimpleExtension[LocalDate]],Optional]
       ]
     ],
     Required
   ]
   with Condition.evidence[
     Condition.EvidenceElement with Condition.Evidence.detail[ObsHistologyProfile],
     Optional
   ]


case class Diagnosis
(
  identifier: NonEmptyList[Identifier],
  subject: Reference[MTBPatient],
  recordedDate: Option[LocalDate],
  code: Option[BasicCodeableConcept[ICD10GM]],
  bodySite: Option[List[BasicCodeableConcept[ICDO3T]]],
  stage: (
           List[Diagnosis.Stage[dtos.Diagnosis.Status.Value]],
           Option[Diagnosis.Stage[WHOGrade.Value]]
         ),
  evidence: Option[List[Diagnosis.HistologyEvidence]]
)
extends DiagnosisProfile



object Diagnosis
{

  import org.hl7.fhir.r4.json._
  import org.hl7.fhir.r4.json.backboneElements._


  object Stage
  {
    final case class Date(value: LocalDate) extends SimpleExtension[LocalDate]

    implicit val url    = Extension.Url[Date]("tumor-stage-date")
    implicit val format = json.extensions.format(Date)
  }

  case class Stage[T](
    extension: Option[Tuple1[Stage.Date]],
    summary: BasicCodeableConcept[T]
  )
  extends Condition.StageElement
     with Condition.Stage.summary[BasicCodeableConcept[T]]
     with Condition.Stage.extensions[Tuple1[Stage.Date],Optional]


  case class HistologyEvidence(
    detail: NonEmptyList[Reference[ObsHistology]]
  )
  extends Condition.EvidenceElement
     with Condition.Evidence.detail[ObsHistology]
 

  implicit val profiles =
    Meta.Profiles[Diagnosis]("http://bwhc.de/mtb/diagnosis")
    
  implicit val formatEvidence = Json.format[HistologyEvidence]

  implicit def formatStage[T: Coding.System] = Json.format[Stage[T]]

  implicit val formatDiagnosis = Json.format[Diagnosis]
  
}
