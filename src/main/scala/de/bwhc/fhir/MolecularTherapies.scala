package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4._
import org.hl7.fhir.r4.MedicationStatement._

import org.hl7.fhir.r4.json._
import org.hl7.fhir.r4.json.contained._

import play.api.libs.json._

import de.bwhc.mtb.data.entry.dtos.MolecularTherapy.{
  StopReason,
  NotDoneReason
}


sealed trait DosageDensityProfile
extends Dosage
   with Dosage.doseAndRate[
     Dosage.DoseAndRateElement
     with Dosage.DoseAndRate.doseRange[
       Range
       with Range.low[SimpleQuantity,Required]
       with Range.high[SimpleQuantity,Required],
       Required 
     ],
     Required 
   ]

final case class DosageRange
(
  doseRange: BasicRange
)
extends Dosage.DoseAndRateElement 
   with Dosage.DoseAndRate.doseRange[BasicRange,Required]

final case class DosageDensity
(
  val doseAndRate: DosageRange
)
extends DosageDensityProfile

object DosageDensity
{
  implicit val formatDosageRange = Json.format[DosageRange]
  implicit val format            = Json.format[DosageDensity]
}


final case class Note(text: String) extends Annotation
object Note
{
  implicit val format = Json.format[Note]
}


sealed trait MolecularTherapyProfile
extends MedicationStatement
   with MedicationStatement.identifierNel
   with MedicationStatement.subject[Patient,Required]
//   with MedicationStatement.reasonReferenceNel[Diagnosis]
   with MedicationStatement.basedOnNel[TherapyRecommendationProfile]
   with MedicationStatement.dateAsserted[LocalDate,Required]
   with MedicationStatement.contained[
     ContainedResources {
       val medication: MTBMedicationProfile
     }
   ]
   with MedicationStatement.effectivePeriod[Period[LocalDate],Optional]
   with MedicationStatement.dosage[DosageDensityProfile,Optional]
   with MedicationStatement.statusReason[CodeableConceptDynamic,Optional]
//   with MedicationStatement.statusReason[
//     CodeableConcept with CodeableConcept.coding[CodingDynamic,Optional], Optional
//   ]
   with MedicationStatement.medicationReference[MTBMedicationProfile]
   with MedicationStatement.note[Annotation,Optional]



object MolecularTherapy
{

  implicit val profile =
    Meta.Profiles[MolecularTherapy]("http://bwhc.de/mtb/molecular-therapy")

  object Systems
  {
    implicit val stopReasonSystem =
      CodingSystem[StopReason.Value]("http://bwhc.de/mtb/molecular-therapy/stop-reason")

    implicit val notDoneReasonSystem =
      CodingSystem[NotDoneReason.Value]("http://bwhc.de/mtb/molecular-therapy/not-done-reason")
  }

  import Systems._

  implicit val format = Json.format[MolecularTherapy]

}


final case class MolecularTherapy
(
  identifier: NonEmptyList[Identifier],
  contained: ContainedMedication,
  basedOn: NonEmptyList[LogicalReference[TherapyRecommendation]],
  status: MedicationStatement.Status.Value,
  dateAsserted: LocalDate,
  subject: LogicalReference[MTBPatient],
//  reasonReference: NonEmptyList[LogicalReference[Diagnosis]],
  medicationReference: LiteralReference[MTBMedication],
  effectivePeriod: Option[Period[LocalDate]],
  dosage: Option[List[DosageDensity]],
  statusReason: Option[List[CodeableConceptDynamic]],
//  statusReason: NonEmptyList[CodeableConceptStatic[StopReason.Value]],
  note: Option[List[Note]],
)
extends MolecularTherapyProfile


trait MolecularTherapyHistoryProfile
extends Bundle.History[MolecularTherapy]

final case class MolecularTherapyHistory
(
  entry: Bundle.History.Entries[Bundle.EntryOf[MolecularTherapy]]
)
extends MolecularTherapyHistoryProfile

object MolecularTherapyHistory
{
  implicit val profiles =
    Meta.Profiles[MolecularTherapyHistory]("http://bwhc.de/mtb/molecular-therapy-history")

  implicit val format = Json.format[MolecularTherapyHistory]
}

/*
sealed trait MolecularTherapy
extends MedicationStatement
   with MedicationStatement.identifierNel
   with MedicationStatement.subject[Patient,Required]
//   with MedicationStatement.reasonReferenceNel[Diagnosis]
   with MedicationStatement.basedOnNel[TherapyRecommendationProfile]
   with MedicationStatement.dateAsserted[LocalDate,Required]
   with MedicationStatement.medicationReference[MTBMedicationProfile]
   with MedicationStatement.note[Annotation,Optional]


final case class Note(text: String) extends Annotation
object Note
{
  implicit val format = Json.format[Note]
}


sealed trait DosageDensityProfile
extends Dosage
   with Dosage.doseAndRate[
     Dosage.DoseAndRateElement
     with Dosage.DoseAndRate.doseRange[
       Range
       with Range.low[SimpleQuantity,Required]
       with Range.high[SimpleQuantity,Required],
       Required 
     ],
     Required 
   ]


final case class DosageRange
(
  doseRange: BasicRange
)
extends Dosage.DoseAndRateElement 
   with Dosage.DoseAndRate.doseRange[BasicRange,Required]

final case class DosageDensity
(
  val doseAndRate: DosageRange
)
extends DosageDensityProfile

object DosageDensity
{
  implicit val formatDosageRange = Json.format[DosageRange]
  implicit val format            = Json.format[DosageDensity]
}



object MolecularTherapy
{

  implicit def profile[Th <: MolecularTherapy] =
    Meta.Profiles[Th]("http://bwhc.de/mtb/molecular-therapy")

  object Systems
  {
    implicit val stopReasonSystem =
      CodingSystem[StopReason.Value]("http://bwhc.de/mtb/molecular-therapy/stop-reason")

    implicit val notDoneReasonSystem =
      CodingSystem[NotDoneReason.Value]("http://bwhc.de/mtb/molecular-therapy/not-done-reason")
  }

  import Systems._


  implicit val formatNotTaken  = Json.format[NotTakenMolecularTherapy]
  implicit val formatStopped   = Json.format[StoppedMolecularTherapy]
  implicit val formatCompleted = Json.format[CompletedMolecularTherapy]
  implicit val formatActive    = Json.format[ActiveMolecularTherapy]

  import MedicationStatement.Status._

  implicit val format: Format[MolecularTherapy] =
    Format[MolecularTherapy](
      Reads(js =>
        (js \ "status")
          .validate[MedicationStatement.Status.Value]
          .flatMap {
            case NotTaken  => js.validate[NotTakenMolecularTherapy]
            case Stopped   => js.validate[StoppedMolecularTherapy]
            case Completed => js.validate[CompletedMolecularTherapy]
            case Active    => js.validate[ActiveMolecularTherapy]
          }
      ), 
      Writes {
        mth =>
          val js = mth match {
            case th: NotTakenMolecularTherapy  => Json.toJson(th)
            case th: StoppedMolecularTherapy   => Json.toJson(th)
            case th: CompletedMolecularTherapy => Json.toJson(th)
            case th: ActiveMolecularTherapy    => Json.toJson(th)
          }
          js.as[JsObject] + ("status" -> Json.toJson(mth.status))
      }
    )

}


final case class NotTakenMolecularTherapy
(
  identifier: NonEmptyList[Identifier],
  basedOn: NonEmptyList[LogicalReference[TherapyRecommendation]],
  dateAsserted: LocalDate,
  subject: LogicalReference[MTBPatient],
  medicationReference: LiteralReference[MTBMedication],
  statusReason: NonEmptyList[CodeableConceptStatic[NotDoneReason.Value]],
  note: Option[List[Note]],
)
extends MolecularTherapy
   with MedicationStatement.statusReasonNel[
     CodeableConcept with CodeableConcept.codingNel[CodingStatic[NotDoneReason.Value]]
   ]
{
  val status: MedicationStatement.Status.Value = MedicationStatement.Status.NotTaken
}

final case class StoppedMolecularTherapy
(
  identifier: NonEmptyList[Identifier],
  contained: ContainedMedication,
  basedOn: NonEmptyList[LogicalReference[TherapyRecommendation]],
  dateAsserted: LocalDate,
  subject: LogicalReference[MTBPatient],
//  reasonReference: NonEmptyList[LogicalReference[Diagnosis]],
  medicationReference: LiteralReference[MTBMedication],
  effectivePeriod: ClosedPeriod[LocalDate],
  dosage: Option[List[DosageDensity]],
  statusReason: NonEmptyList[CodeableConceptStatic[StopReason.Value]],
  note: Option[List[Note]],
)
extends MolecularTherapy
   with MedicationStatement.contained[ContainedResources { val medication: MTBMedicationProfile }]
   with MedicationStatement.effectivePeriod[ClosedPeriod[LocalDate],Required]
   with MedicationStatement.dosage[DosageDensityProfile,Optional]
   with MedicationStatement.statusReasonNel[
     CodeableConcept with CodeableConcept.codingNel[CodingStatic[StopReason.Value]]
   ]
{
  val status: MedicationStatement.Status.Value = MedicationStatement.Status.Stopped
}


final case class CompletedMolecularTherapy
(
  identifier: NonEmptyList[Identifier],
  contained: ContainedMedication,
  basedOn: NonEmptyList[LogicalReference[TherapyRecommendation]],
  dateAsserted: LocalDate,
  subject: LogicalReference[MTBPatient],
//  reasonReference: NonEmptyList[LogicalReference[Diagnosis]],
  medicationReference: LiteralReference[MTBMedication],
  effectivePeriod: ClosedPeriod[LocalDate],
  dosage: Option[List[DosageDensity]],
  note: Option[List[Note]],
)
extends MolecularTherapy
   with MedicationStatement.contained[ContainedResources { val medication: MTBMedicationProfile }]
   with MedicationStatement.effectivePeriod[ClosedPeriod[LocalDate],Required]
   with MedicationStatement.dosage[DosageDensityProfile,Optional]
{
  val status: MedicationStatement.Status.Value = MedicationStatement.Status.Completed
}


final case class ActiveMolecularTherapy
(
  identifier: NonEmptyList[Identifier],
  contained: ContainedMedication,
  basedOn: NonEmptyList[LogicalReference[TherapyRecommendation]],
  dateAsserted: LocalDate,
  subject: LogicalReference[MTBPatient],
//  reasonReference: NonEmptyList[LogicalReference[Diagnosis]],
  medicationReference: LiteralReference[MTBMedication],
  effectivePeriod: OpenEndPeriod[LocalDate],
  dosage: Option[List[DosageDensity]],
  note: Option[List[Note]],
)
extends MolecularTherapy
   with MedicationStatement.contained[ContainedResources { val medication: MTBMedicationProfile }]
   with MedicationStatement.effectivePeriod[OpenEndPeriod[LocalDate],Required]
   with MedicationStatement.dosage[DosageDensityProfile,Optional]
{
  val status: MedicationStatement.Status.Value = MedicationStatement.Status.Active
}



trait MolecularTherapyHistoryProfile
extends Bundle.History[MolecularTherapy]

final case class MolecularTherapyHistory
(
  entry: Bundle.History.Entries[Bundle.EntryOf[MolecularTherapy]]
)
extends MolecularTherapyHistoryProfile

object MolecularTherapyHistory
{
  implicit val profiles =
    Meta.Profiles[MolecularTherapyHistory]("http://bwhc.de/mtb/molecular-therapy-history")

  implicit val format = Json.format[MolecularTherapyHistory]
}
*/
