package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Specimen._
import org.hl7.fhir.r4.Organization._
import org.hl7.fhir.r4.json._

import play.api.libs.json.Json

import de.bwhc.mtb.dtos


import CodingSystems._



final case class SampleDiagnosis(
  value: CodeableConceptStatic[dtos.ICD10GM]
)
extends SimpleExtension[CodeableConceptStatic[dtos.ICD10GM]]

object SampleDiagnosis
{
  implicit val url =
    Extension.Url[SampleDiagnosis]("https://fhir.bbmri.de/StructureDefinition/SampleDiagnosis")

  implicit val format =
    json.extensions.format(SampleDiagnosis(_))
}


trait TumorSpecimenProfile
extends Specimen
   with Specimen.identifierNel
   with Specimen.modifierExtension[SampleDiagnosis]
   with Specimen.`type`[Required]
   with Specimen.subject[Patient,Required]
   with Specimen.collection[
     Specimen.CollectionElement
       with Specimen.Collection.collectedDateTime[LocalDate,Required]
       with Specimen.Collection.bodySite[
         CodeableConcept
           with CodeableConcept.codingNel[CodingStatic[dtos.Specimen.Collection.Localization.Value]],
//           with CodeableConcept.codingNel[Coding[dtos.Specimen.Collection.Localization.Value]],
         Required
       ]
       with Specimen.Collection.method[
         CodeableConcept
           with CodeableConcept.codingNel[CodingStatic[dtos.Specimen.Collection.Method.Value]],
//           with CodeableConcept.codingNel[Coding[dtos.Specimen.Collection.Method.Value]],
         Required
       ],
     Optional
   ]
   with Specimen.condition[
     CodeableConcept
     with CodeableConcept.codingNel[CodingStatic[dtos.Specimen.Type.Value]],
     Optional
   ]



final case class TumorSpecimen
(
  identifier: NonEmptyList[Identifier],
  modifierExtension: NonEmptyList[SampleDiagnosis],
  subject: LogicalReference[MTBPatient],
  collection: Option[TumorSpecimen.Collection],
  condition: Option[List[CodeableConceptStatic[dtos.Specimen.Type.Value]]],
  `type`: CodeableConceptStatic[HL7v2Table0487] =
     CodeableConceptStatic(CodingStatic[HL7v2Table0487]("TUMOR",Some("Tumor"),None)) 
) 
extends TumorSpecimenProfile


object TumorSpecimen
{

  final case class Collection 
  (
    collectedDateTime: LocalDate,
    bodySite: CodeableConceptStatic[dtos.Specimen.Collection.Localization.Value],
    method: CodeableConceptStatic[dtos.Specimen.Collection.Method.Value]
  )
  extends Specimen.CollectionElement
     with Specimen.Collection.collectedDateTime[LocalDate,Required]
     with Specimen.Collection.bodySite[CodeableConceptStatic[dtos.Specimen.Collection.Localization.Value],Required]
     with Specimen.Collection.method[CodeableConceptStatic[dtos.Specimen.Collection.Method.Value],Required]


  implicit val profiles =
    Meta.Profiles[TumorSpecimen]("http://bwhc.de/mtb/tumor-specimen")

  implicit val typeSystem =
    CodingSystem[dtos.Specimen.Type.Value]("http://bwhc.de/mtb/tumor-specimen/type")
    
  implicit val localizationSystem =
    CodingSystem[dtos.Specimen.Collection.Localization.Value]("http://bwhc.de/mtb/tumor-specimen/localization")
    
  implicit val methodSystem =
    CodingSystem[dtos.Specimen.Collection.Method.Value]("http://bwhc.de/mtb/tumor-specimen/collection-method")



  implicit val formatCollection = Json.format[Collection]

  implicit val format = Json.format[TumorSpecimen]
  
}

