package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList


import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Observation._
import org.hl7.fhir.r4.DiagnosticReport._
import org.hl7.fhir.r4.json._

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.TumorCellContent
import de.bwhc.mtb.data.entry.dtos.SomaticNGSReport.SequencingType


//-----------------------------------------------------------------------------
// Tumor Content
//-----------------------------------------------------------------------------

abstract class ObsTumorCellContentProfile
//extends Observation
extends ObservationSC
   with Observation.id[Required]
   with Observation.subject[Patient,Required]
   with Observation.specimen[Required]
   with Observation.method[
     CodeableConcept
       with CodeableConcept.codingNel[CodingStatic[TumorCellContent.Method.Value]],
     Required
   ]
   with Observation.valueQuantity[Quantity,Required]


case class ObsTumorCellContent
(
  id: String,
  status: Observation.Status.Value,
  subject: LogicalReference[MTBPatient],
  specimen: LogicalReference[TumorSpecimen],
  method: CodeableConceptStatic[TumorCellContent.Method.Value],
  valueQuantity: SimpleQuantity
)
extends ObsTumorCellContentProfile

object ObsTumorCellContent
{

  implicit val profiles =
    Meta.Profiles[ObsTumorCellContent]("http://bwhc.de/mtb/observation-tumor-content")

  implicit val code =
    Code[ObsTumorCellContent](LOINC("TODO: LOINC tumor-content", Some("Tumor Content")))

  implicit val tcMethodSystem =
    CodingSystem[TumorCellContent.Method.Value]("tumor-content-method")
    
  implicit val format = Json.format[ObsTumorCellContent]
  
}


//-----------------------------------------------------------------------------
// Tumor Mutational Burden (TMB)
//-----------------------------------------------------------------------------

abstract class ObsTMBProfile
extends ObservationSC
//extends Observation
   with Observation.id[Required]
   with Observation.subject[Patient,Required]
   with Observation.specimen[Required]
   with Observation.valueQuantity[Quantity,Required]


case class ObsTMB
(
  id: String,
  status: Observation.Status.Value,
  subject: LogicalReference[MTBPatient],
  specimen: LogicalReference[TumorSpecimen],
  valueQuantity: SimpleQuantity
)
extends ObsTMBProfile

object ObsTMB
{
  implicit val profiles =
    Meta.Profiles[ObsTMB]("http://bwhc.de/mtb/observation-tumor-mutational-burden")

  implicit val code = Code[ObsTMB](LOINC("TODO: LOINC TMB", Some("Tumor Mutational Burden")))

  implicit val format = Json.format[ObsTMB]
}


//-----------------------------------------------------------------------------
// Micro-Satellite Instabilities (MSI)
//-----------------------------------------------------------------------------

abstract class ObsMSIProfile
//extends Observation
extends ObservationSC
   with Observation.id[Required]
   with Observation.subject[Patient,Required]
   with Observation.specimen[Required]
   with Observation.valueQuantity[Quantity,Required]


case class ObsMSI
(
  id: String,
  status: Observation.Status.Value,
  subject: LogicalReference[MTBPatient],
  specimen: LogicalReference[TumorSpecimen],
  valueQuantity: SimpleQuantity
)
extends ObsMSIProfile

object ObsMSI
{
  implicit val profiles =
    Meta.Profiles[ObsMSI]("http://bwhc.de/mtb/observation-msi")

  implicit val code =
    Code[ObsMSI](LOINC("TODO: LOINC MSI", Some("Micro-Satellite Instabilities (MSI)")))

  implicit val format = Json.format[ObsMSI]
}


//-----------------------------------------------------------------------------
// BRCAness
//-----------------------------------------------------------------------------

abstract class ObsBRCAnessProfile
//extends Observation
extends ObservationSC
   with Observation.id[Required]
   with Observation.subject[Patient,Required]
   with Observation.specimen[Required]
   with Observation.valueQuantity[Quantity,Required]


case class ObsBRCAness
(
  id: String,
  status: Observation.Status.Value,
  subject: LogicalReference[MTBPatient],
  specimen: LogicalReference[TumorSpecimen],
  valueQuantity: SimpleQuantity
)
extends ObsBRCAnessProfile

object ObsBRCAness
{
  implicit val profiles = Meta.Profiles[ObsBRCAness]("http://bwhc.de/mtb/observation-brcaness")

  implicit val code =
    Code[ObsBRCAness](LOINC("TODO: LOINC BRCAness", Some("BRCAness")))

  implicit val format = Json.format[ObsBRCAness]
}



//-----------------------------------------------------------------------------
// NGSReport Profile
//-----------------------------------------------------------------------------

final case class ExtSequencingType(
  value: CodingStatic[SequencingType]
) extends SimpleExtension[CodingStatic[SequencingType]]

object ExtSequencingType
{
  import CodingSystems._

  implicit val url    =
    Extension.Url[ExtSequencingType]("http://bwhc.de/mtb/somatic-ngs-report/sequencing-type")

  implicit val format =
    json.extensions.format(ExtSequencingType(_))
}


final case class ExtMetaData
(
  kitType: ExtMetaData.KitType,
  kitManufacturer: ExtMetaData.KitManufacturer,
  sequencer: ExtMetaData.Sequencer,
  refGenome: ExtMetaData.RefGenome,
  pipeline: Option[ExtMetaData.Pipeline]
)
extends Extension

object ExtMetaData
{

  import java.net.URI

  final case class KitType(value: String) extends SimpleExtension[String]
  final case class KitManufacturer(value: String) extends SimpleExtension[String]
  final case class Sequencer(value: String) extends SimpleExtension[String]
  final case class RefGenome(value: String) extends SimpleExtension[String]
  final case class Pipeline(value: URI) extends SimpleExtension[URI]


  implicit val urlKitType = Extension.Url[KitType]("http://bwhc.de/mtb/somatic-ngs-report/metadata/kitType")
  implicit val formatKitType = json.extensions.format(KitType(_))


  implicit val urlKitManufacturer = Extension.Url[KitManufacturer]("http://bwhc.de/mtb/somatic-ngs-report/metadata/kitManufacturer")
  implicit val formatKitManufacture = json.extensions.format(KitManufacturer(_))


  implicit val urlSequencer = Extension.Url[Sequencer]("http://bwhc.de/mtb/somatic-ngs-report/metadata/sequencer")
  implicit val formatSequencer = json.extensions.format(Sequencer(_))


  implicit val urlRefGenome = Extension.Url[RefGenome]("http://bwhc.de/mtb/somatic-ngs-report/metadata/refGenome")
  implicit val formatRefGenome = json.extensions.format(RefGenome(_))


  implicit val urlPipeline = Extension.Url[Pipeline]("http://bwhc.de/mtb/somatic-ngs-report/metadata/pipeline")
  implicit val formatPipeline = json.extensions.format(Pipeline(_))


  implicit val url = Extension.Url[ExtMetaData]("http://bwhc.de/mtb/somatic-ngs-report/metadata")

  import json.extensions._

  implicit val format = json.extensions.format[ExtMetaData]

}


trait SomaticNGSReportProfile
extends DiagnosticReportSC
   with DiagnosticReport.identifierNel
   with DiagnosticReport.subject[Patient,Required]
   with DiagnosticReport.specimenNel[TumorSpecimenProfile]
   with DiagnosticReport.issued[LocalDate,Required]
   with DiagnosticReport.extensions[
     ExtensionSet {
       val sequencingType: ExtSequencingType
       val metadata: List[ExtMetaData]
     },
     Required
   ]
//   with DiagnosticReport.resultNel[Observation]
   with DiagnosticReport.result[Observation,Required]
   with DiagnosticReport.contained[
     ContainedResources {
       val tumorCellContent:   Option[ObsTumorCellContentProfile]
       val tmb:                Option[ObsTMBProfile]
       val msi:                Option[ObsMSIProfile]
       val brcaness:           Option[ObsBRCAnessProfile]
       val simpleVariants:     List[SimpleVariantProfile]
       val copyNumberVariants: List[CopyNumberVariantProfile]
     }
   ]


final case class SomaticNGSReport
(
  identifier: NonEmptyList[Identifier],
  issued: LocalDate,
  status: DiagnosticReport.Status.Value,
  extension: SomaticNGSReport.Extensions,
  subject: LogicalReference[MTBPatient],
  specimen: NonEmptyList[LogicalReference[TumorSpecimen]],
//  result: NonEmptyList[LiteralReference[Observation]],
  result: List[LiteralReference[Observation]],
  contained: SomaticNGSReport.Results
)
extends SomaticNGSReportProfile


object SomaticNGSReport
{

  implicit val profile =
   Meta.Profiles[SomaticNGSReport]("http://bwhc.de/mtb/somatic-ngs-report")

  implicit val code =
    Code[SomaticNGSReport](LOINC("TODO: LOINC Code NGS Report",Some("Somatic NGS Report")))

  
  final case class Extensions
  (
    sequencingType: ExtSequencingType,
    metadata: List[ExtMetaData]
  )
  extends ExtensionSet

  object Extensions {

    import json.extensions._

    implicit val format = json.extensions.format[Extensions]
  }


  final case class Results
  (
    tumorCellContent:   Option[ObsTumorCellContent],
    tmb:                Option[ObsTMB],
    msi:                Option[ObsMSI],
    brcaness:           Option[ObsBRCAness],
    simpleVariants:     List[SimpleVariant],
    copyNumberVariants: List[CopyNumberVariant]
  )
  extends ContainedResources


  object Results {

    import json.contained._

    implicit val format = json.contained.format[Results]
  }



  implicit val format = Json.format[SomaticNGSReport]

}
