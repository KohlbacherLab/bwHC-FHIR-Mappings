package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Observation._

import play.api.libs.json.Json


abstract class ObsVariant extends ObservationSC

object ObsVariant
{

  sealed trait ClinVar
  object ClinVar
  {
    implicit val system = CodingSystem[ClinVar]("https://www.ncbi.nlm.nih.gov/clinvar/")
  }

  sealed trait HGNC
  object HGNC
  {
    implicit val system = CodingSystem[HGNC]("https://www.genenames.org/")
  }

  sealed trait HGVS
  object HGVS
  {
    implicit val system = CodingSystem[HGVS]("https://www.hgvs.org")
  }

  sealed trait dbSNP
  object dbSNP
  {
    implicit val system = CodingSystem[dbSNP]("https://www.ncbi.nlm.nih.gov/snp/")
  }

  sealed trait SequenceOntology
  object SequenceOntology
  {
    implicit val system = CodingSystem[SequenceOntology]("http://www.sequenceontology.org")
  }

  sealed trait TBD_LOINC
  object TBD_LOINC
  {
    implicit val system = CodingSystem[TBD_LOINC]("http://hl7.org/fhir/uv/genomics-reporting/CodeSystem/tbd-codes")
  }



  final case class Chromosome(valueString: String)
  extends Observation.ComponentElementSC
     with Observation.Component.valueString[Required]
 

  final case class GeneStudied(valueCodeableConcept: CodeableConceptStatic[HGNC])
  extends Observation.ComponentElementSC
     with Observation.Component.valueCodeableConcept[
       CodeableConcept with CodeableConcept.codingNel[CodingStatic[HGNC]],
       Required
     ]
 

  final case class FunctionalAnnotation(valueCodeableConcept: CodeableConceptStatic[SequenceOntology])
  extends Observation.ComponentElementSC
     with Observation.Component.valueCodeableConcept[
       CodeableConcept with CodeableConcept.codingNel[CodingStatic[SequenceOntology]],
       Required
     ]

 
  final case class ExactStartEnd(valueRange: LBoundedRange)
  extends Observation.ComponentElementSC
     with Observation.Component.valueRange[LBoundedRange,Required]
//  final case class ExactStartEnd(valueString: String)
//  extends Observation.ComponentElement
//     with Observation.Component.valueString[Required]
 
 
  final case class RefAllele(valueString: String)
  extends Observation.ComponentElementSC
     with Observation.Component.valueString[Required]
 
 
  final case class AltAllele(valueString: String)
  extends Observation.ComponentElementSC
     with Observation.Component.valueString[Required]
 
 
  final case class AminoAcidChange(valueCodeableConcept: CodeableConceptStatic[HGVS])
  extends Observation.ComponentElementSC
     with Observation.Component.valueCodeableConcept[
       CodeableConcept with CodeableConcept.codingNel[CodingStatic[HGVS]],
       Required
     ]
 
 
  final case class DNAChange(valueCodeableConcept: CodeableConceptStatic[HGVS])
  extends Observation.ComponentElementSC
     with Observation.Component.valueCodeableConcept[
       CodeableConcept with CodeableConcept.codingNel[CodingStatic[HGVS]],
       Required
     ]
 
 
  final case class DbSNPId(valueCodeableConcept: CodeableConceptStatic[dbSNP])
  extends Observation.ComponentElementSC
     with Observation.Component.valueCodeableConcept[
       CodeableConcept with CodeableConcept.codingNel[CodingStatic[dbSNP]],
       Required
     ]
 
 
  final case class SampleAllelicFrequency(valueQuantity: SimpleQuantity)
  extends Observation.ComponentElementSC
     with Observation.Component.valueQuantity[Quantity,Required]
 
 
  final case class AllelicReadDepth(valueQuantity: SimpleQuantity)
  extends Observation.ComponentElementSC
     with Observation.Component.valueQuantity[Quantity,Required]

 


  object component
  { 

    trait chromosome[C[_]]{
      val chromosome: C[Chromosome]
    }
 
    trait geneStudied[C[_]]{
      val geneStudied: C[List[GeneStudied]]
    }
    trait geneStudiedNel{
      val geneStudied: NonEmptyList[GeneStudied]
    }
 
    trait functionalAnnotation[C[_]]{
      val functionalAnnotation: C[List[FunctionalAnnotation]]
    }
    trait functionalAnnotationNel{
      val functionalAnnotation: NonEmptyList[FunctionalAnnotation]
    }
  
    trait exactStartEnd[C[_]]{
      val exactStartEnd: C[ExactStartEnd]
    }
  
    trait refAllele[C[_]]{
      val refAllele: C[RefAllele]
    }
 
    trait altAllele[C[_]]{
      val altAllele: C[AltAllele]
    }
 
    trait dnaChange[C[_]]{
      val dnaChange: C[DNAChange]
    }
 
    trait aminoAcidChange[C[_]]{
      val aminoAcidChange: C[AminoAcidChange]
    }
 
    trait dbSNPId[C[_]]{
      val dbSNPId: C[DbSNPId]
    }
 
    trait sampleAllelicFrequency[C[_]]{
      val sampleAllelicFrequency: C[SampleAllelicFrequency]
    }
 
    trait allelicReadDepth[C[_]]{
      val allelicReadDepth: C[AllelicReadDepth]
    } 

  }


  implicit val codeChromosome =
    Code[Chromosome](LOINC("48000-4",Some("Chromosome")))

  implicit val codeGeneStudied =
    Code[GeneStudied](LOINC("48018-6",Some("GeneStudied")))
 
  implicit val codeFunctionalAnnotation =
    Code[FunctionalAnnotation,TBD_LOINC]("functional-annotation",Some("FunctionalAnnotation"))
 
  implicit val codeExactStartEnd =
    Code[ExactStartEnd,TBD_LOINC]("exact-start-end",Some("Exact start-end"))
 
  implicit val codeRefAllele =
    Code[RefAllele](LOINC("69547-8",Some("RefAllele")))
 
  implicit val codeAltAllele =
    Code[AltAllele](LOINC("69551-0",Some("AltAllele")))
 
  implicit val codeAminoAcidChange =
    Code[AminoAcidChange](LOINC("48005-3",Some("AminoAcidChange")))
 
  implicit val codeDNAChange =
    Code[DNAChange](LOINC("48004-6",Some("DNAChange")))
 
  implicit val codeDbSNPId =
    Code[DbSNPId](LOINC("81255-2",Some("dbSNPId")))
 
  implicit val codeSampleAllFreq =
    Code[SampleAllelicFrequency](LOINC("81258-6",Some("SampleAllelicFrequency")))
 
  implicit val codeAllelicReadDepth =
    Code[AllelicReadDepth](LOINC("82121-5",Some("AllelicReadDepth")))


  implicit val formatChromosome             = Json.format[Chromosome]
  implicit val formatGeneStudied            = Json.format[GeneStudied]
  implicit val formatFunctionalAnnotation   = Json.format[FunctionalAnnotation]
  implicit val formatExactStartEnd          = Json.format[ExactStartEnd]
  implicit val formatRefAllele              = Json.format[RefAllele]
  implicit val formatAltAllele              = Json.format[AltAllele]
  implicit val formatAminoAcidChange        = Json.format[AminoAcidChange]
  implicit val formatDNAChange              = Json.format[DNAChange]
  implicit val formatDBSNPID                = Json.format[DbSNPId]
  implicit val formatSampleAllelicFrequency = Json.format[SampleAllelicFrequency]
  implicit val formatAllelicReadDepth       = Json.format[AllelicReadDepth]

}


abstract class SomaticVariantProfile
extends ObsVariant
   with Observation.id[Required]
   with Observation.identifier[Optional]
   with Observation.subject[Patient,Required]
   with Observation.interpretationNel[
     CodeableConcept
     with CodeableConcept.codingNel[CodingStatic[ObsVariant.ClinVar]]
   ]


abstract class SimpleVariantProfile
extends SomaticVariantProfile
   with Observation.components[
     Product
     with ObsVariant.component.chromosome[Required]
     with ObsVariant.component.geneStudiedNel
     with ObsVariant.component.exactStartEnd[Required]
     with ObsVariant.component.refAllele[Required]
     with ObsVariant.component.altAllele[Required]
     with ObsVariant.component.aminoAcidChange[Required]
     with ObsVariant.component.dnaChange[Required]
     with ObsVariant.component.sampleAllelicFrequency[Required]
     with ObsVariant.component.allelicReadDepth[Required]
     with ObsVariant.component.dbSNPId[Optional],
     Required
   ]



final case class SimpleVariant
(
  id: String,
  identifier: Option[List[Identifier]],
  status: Observation.Status.Value,
  subject: LogicalReference[Patient],
  component: SimpleVariant.Components,
  interpretation: NonEmptyList[CodeableConceptStatic[ObsVariant.ClinVar]]
)
extends SimpleVariantProfile

object SimpleVariant
{

  import ObsVariant._

  final case class Components
  (
    chromosome: Chromosome,
    geneStudied: NonEmptyList[GeneStudied],
    exactStartEnd: ExactStartEnd,
    refAllele: RefAllele,
    altAllele: AltAllele,
    dnaChange: DNAChange,
    aminoAcidChange: AminoAcidChange,
    dbSNPId: Option[DbSNPId],
    sampleAllelicFrequency: SampleAllelicFrequency,
    allelicReadDepth: AllelicReadDepth
  )
  extends ObsVariant.component.geneStudiedNel
     with ObsVariant.component.chromosome[Required]
     with ObsVariant.component.exactStartEnd[Required]
     with ObsVariant.component.refAllele[Required]
     with ObsVariant.component.altAllele[Required]
     with ObsVariant.component.dnaChange[Required]
     with ObsVariant.component.aminoAcidChange[Required]
     with ObsVariant.component.dbSNPId[Optional]
     with ObsVariant.component.sampleAllelicFrequency[Required]
     with ObsVariant.component.allelicReadDepth[Required]


  implicit val profile =
    Meta.Profiles[SimpleVariant]("http://bwhc.de/mtb/genetics-simple-somatic-variant")

  implicit val code =
    Code[SimpleVariant](LOINC("69548-6"))


  import org.hl7.fhir.r4.json._
  import json.backboneElements._

  implicit val format = Json.format[SimpleVariant]

}

