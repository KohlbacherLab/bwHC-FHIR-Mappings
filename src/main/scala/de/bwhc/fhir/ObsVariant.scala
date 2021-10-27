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

 
/*
final case class CNV
(
  id: Variant.Id,
  chromosome: Chromosome,
  startRange: StartEnd,
  endRange: StartEnd,
  totalCopyNumber: Int,
  relativeCopyNumber: Double,
  cnA: Option[Double],
  cnB: Option[Double],
  reportedAffectedGenes: Option[List[Coding[Gene]]],
  reportedFocality: Option[String],
  `type`: CNV.Type.Value,
  copyNumberNeutralLoH: Option[List[Coding[Gene]]],
)
extends Variant
*/

  // CNV components
  final case class StartRange(valueRange: LBoundedRange)
  extends Observation.ComponentElementSC
     with Observation.Component.valueRange[LBoundedRange,Required]
 
  final case class EndRange(valueRange: LBoundedRange)
  extends Observation.ComponentElementSC
     with Observation.Component.valueRange[LBoundedRange,Required]
 
  final case class CopyNumber(valueInteger: Int)
  extends Observation.ComponentElementSC
     with Observation.Component.valueInteger[Required]

  final case class RelativeCopyNumber(valueQuantity: SimpleQuantity)
  extends Observation.ComponentElementSC
     with Observation.Component.valueQuantity[Quantity,Required]

  final case class CnA(valueQuantity: SimpleQuantity)
  extends Observation.ComponentElementSC
     with Observation.Component.valueQuantity[Quantity,Required]

  final case class CnB(valueQuantity: SimpleQuantity)
  extends Observation.ComponentElementSC
     with Observation.Component.valueQuantity[Quantity,Required]

  final case class ReportedAffectedGene(valueCodeableConcept: CodeableConceptStatic[HGNC])
  extends Observation.ComponentElementSC
     with Observation.Component.valueCodeableConcept[
       CodeableConcept with CodeableConcept.codingNel[CodingStatic[HGNC]],
       Required
     ]

  final case class ReportedFocality(valueString: String)
  extends Observation.ComponentElementSC
     with Observation.Component.valueString[Required]

/*
  final case class CNVType(valueCodeableConcept: CodeableConceptStatic[])
  extends Observation.ComponentElementSC
     with Observation.Component.valueCodeableConcept[
       CodeableConcept with CodeableConcept.codingNel[CodingStatic[]],
       Required
     ]
*/

  final case class CopyNumberNeutralLoH(valueCodeableConcept: CodeableConceptStatic[HGNC])
  extends Observation.ComponentElementSC
     with Observation.Component.valueCodeableConcept[
       CodeableConcept with CodeableConcept.codingNel[CodingStatic[HGNC]],
       Required
     ]


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


    // CNV components
    trait startRange[C[_]]{
      val startRange: C[StartRange]
    }
  
    trait endRange[C[_]]{
      val endRange: C[EndRange]
    }
  
    trait copyNumber[C[_]]{
      val copyNumber: C[CopyNumber]
    }

    trait relativeCopyNumber[C[_]]{
      val relativeCopyNumber: C[RelativeCopyNumber]
    }
 
    trait cnA[C[_]]{
      val cnA: C[CnA]
    }
 
    trait cnB[C[_]]{       
      val cnB: C[CnB]
    }
 
    trait reportedAffectedGenes[C[_]]{
      val reportedAffectedGenes: C[List[ReportedAffectedGene]]
    }
    trait reportedAffectedGenesNel{
      val reportedAffectedGenes: NonEmptyList[ReportedAffectedGene]
    }
 
    trait reportedFocality[C[_]]{
      val reportedFocality: C[ReportedFocality]
    }

    trait copyNumberNeutralLoH[C[_]]{
      val copyNumberNeutralLoH: C[List[CopyNumberNeutralLoH]]
    }
    trait copyNumberNeutralLoHNel{
      val copyNumberNeutralLoH: NonEmptyList[CopyNumberNeutralLoH]
    }

  }

  implicit val codeChromosome =
    Code[Chromosome](LOINC("48000-4","Chromosome"))

  implicit val codeGeneStudied =
    Code[GeneStudied](LOINC("48018-6","Gene Studied"))
 
  implicit val codeFunctionalAnnotation =
    Code[FunctionalAnnotation,TBD_LOINC]("functional-annotation","Functional Annotation")
 
  implicit val codeExactStartEnd =
    Code[ExactStartEnd,TBD_LOINC]("exact-start-end","Exact start-end")
 
  implicit val codeRefAllele =
    Code[RefAllele](LOINC("69547-8","RefAllele"))
 
  implicit val codeAltAllele =
    Code[AltAllele](LOINC("69551-0","AltAllele"))
 
  implicit val codeAminoAcidChange =
    Code[AminoAcidChange](LOINC("48005-3","Amino Acid Change"))
 
  implicit val codeDNAChange =
    Code[DNAChange](LOINC("48004-6","DNA Change"))
 
  implicit val codeDbSNPId =
    Code[DbSNPId](LOINC("81255-2","dbSNPId"))
 
  implicit val codeSampleAllFreq =
    Code[SampleAllelicFrequency](LOINC("81258-6","Sample Allelic Frequency"))
 
  implicit val codeAllelicReadDepth =
    Code[AllelicReadDepth](LOINC("82121-5","Allelic Read Depth"))



  implicit val codeStartRange =
    Code[StartRange,TBD_LOINC]("start-range","Start Range")
 
  implicit val codeEndRange =
    Code[EndRange,TBD_LOINC]("end-range","End Range")
 
  implicit val codeCopyNumber =
    Code[CopyNumber](LOINC("82155-3","Copy Number"))

  implicit val codeRelativeCopyNumber =
    Code[RelativeCopyNumber,TBD_LOINC]("relative-copy-number","Relative Copy Number")

  implicit val codeCnA =
    Code[CnA,TBD_LOINC]("cnA","cnA")

  implicit val codeCnB =
    Code[CnB,TBD_LOINC]("cnB","cnB")

  implicit val codeReportedAffectedGene =
    Code[ReportedAffectedGene,TBD_LOINC]("reported-affected-gene","Reported Affected Gene")

  implicit val codeReportedFocality =
    Code[ReportedFocality,TBD_LOINC]("reported-focality","Reported Focality")

  implicit val codeCopyNumberNeutralLoH =
    Code[CopyNumberNeutralLoH,TBD_LOINC]("copy-number-neutral-loh","Copy Number Neutral LoH")



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

  implicit val formatStartRange             = Json.format[StartRange]
  implicit val formatEndRange               = Json.format[EndRange]
  implicit val formatCopyNumber             = Json.format[CopyNumber]
  implicit val formatRelativeCopyNumber     = Json.format[RelativeCopyNumber]
  implicit val formatCnA                    = Json.format[CnA]
  implicit val formatCnB                    = Json.format[CnB]
  implicit val formatReportedAffectedGene   = Json.format[ReportedAffectedGene]
  implicit val formatReportedFocality       = Json.format[ReportedFocality]
  implicit val formatCopyNumberNeutralLoH   = Json.format[CopyNumberNeutralLoH]

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



//TODO: add HGNC-ID?
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


/*
final case class CNV
(
  id: Variant.Id,
  chromosome: Chromosome,
  startRange: StartEnd,
  endRange: StartEnd,
  totalCopyNumber: Int,
  relativeCopyNumber: Double,
  cnA: Option[Double],
  cnB: Option[Double],
  reportedAffectedGenes: Option[List[Coding[Gene]]],
  reportedFocality: Option[String],
  `type`: CNV.Type.Value,
  copyNumberNeutralLoH: Option[List[Coding[Gene]]],
)
extends Variant
*/

abstract class CNVProfile
extends SomaticVariantProfile
   with Observation.components[
     Product
       with ObsVariant.component.chromosome[Required]
       with ObsVariant.component.startRange[Required]
       with ObsVariant.component.endRange[Required]
       with ObsVariant.component.copyNumber[Required]
       with ObsVariant.component.relativeCopyNumber[Required]
       with ObsVariant.component.cnA[Optional]
       with ObsVariant.component.cnB[Optional]
       with ObsVariant.component.reportedAffectedGenes[Required]
//       with ObsVariant.component.reportedAffectedGenes[Optional]
       with ObsVariant.component.reportedFocality[Optional]
       with ObsVariant.component.copyNumberNeutralLoH[Required],
//       with ObsVariant.component.copyNumberNeutralLoH[Optional],
     Required
   ]


final case class CNV
(
  id: String,
  identifier: Option[List[Identifier]],
  status: Observation.Status.Value,
  subject: LogicalReference[Patient],
  component: CNV.Components,
  interpretation: NonEmptyList[CodeableConceptStatic[ObsVariant.ClinVar]]
)
extends CNVProfile


object CNV
{

  import ObsVariant._

  final case class Components
  (
    chromosome: Chromosome,
    startRange: StartRange,
    endRange: EndRange,
    copyNumber: CopyNumber,
    relativeCopyNumber: RelativeCopyNumber,
    cnA: Option[CnA],
    cnB: Option[CnB],
    reportedAffectedGenes: List[ReportedAffectedGene],
//    reportedAffectedGenes: Option[List[ReportedAffectedGene]],
    reportedFocality: Option[ReportedFocality],
    copyNumberNeutralLoH: List[CopyNumberNeutralLoH]
//    copyNumberNeutralLoH: Option[List[CopyNumberNeutralLoH]]
  ) 
  extends ObsVariant.component.chromosome[Required]
     with ObsVariant.component.startRange[Required]
     with ObsVariant.component.endRange[Required]
     with ObsVariant.component.copyNumber[Required]
     with ObsVariant.component.relativeCopyNumber[Required]
     with ObsVariant.component.cnA[Optional]
     with ObsVariant.component.cnB[Optional]
     with ObsVariant.component.reportedAffectedGenes[Required]
//     with ObsVariant.component.reportedAffectedGenes[Optional]
     with ObsVariant.component.reportedFocality[Optional]
     with ObsVariant.component.copyNumberNeutralLoH[Required]
//     with ObsVariant.component.copyNumberNeutralLoH[Optional]


  implicit val profile =
    Meta.Profiles[CNV]("http://bwhc.de/mtb/genetics-copy-number-variant")

  implicit val code =
    Code[CNV,TBD_LOINC]("copy-number-Variant","Copy Number Variant")


  import org.hl7.fhir.r4.json._
  import json.backboneElements._

  implicit val format = Json.format[CNV]

}
