package de.bwhc.fhir


import java.time.LocalDate

import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Bundle.
{
  Entry, EntryElement, EntryOf
}

import play.api.libs.json.Json

import cats.data.NonEmptyList



trait MTBFileBundleProfile
extends Bundle.Collection
   with Bundle.identifier[Required]
   with Bundle.entry[MTBFileEntriesProfile]
  

trait MTBFileEntriesProfile
extends Bundle.EntrySet
{
  this: Product =>

  val patient:                EntryElement with Entry.resource[MTBPatientProfile]
  val diagnoses:              NonEmptyList[EntryElement with Entry.resource[DiagnosisProfile]]
  val previousGLTherapies:    List[EntryElement with Entry.resource[GuidelineTherapyProfile]]
  val lastGLTherapy:          EntryElement with Entry.resource[LastGuidelineTherapyProfile]
  val ecogs:                  List[EntryElement with Entry.resource[ObsECOGProfile]]
  val responses:              List[EntryElement with Entry.resource[ObsRECISTProfile]]
  val specimens:              List[EntryElement with Entry.resource[TumorSpecimenProfile]]
  val histology:              List[EntryElement with Entry.resource[ObsHistologyProfile]]
  val ngsReports:             List[EntryElement with Entry.resource[SomaticNGSReportProfile]]
  val carePlans:              List[EntryElement with Entry.resource[MTBCarePlanProfile]]
  val therapyRecommendations: List[EntryElement with Entry.resource[TherapyRecommendationProfile]]
  val molecularTherapies:     List[EntryElement with Entry.resource[MolecularTherapyHistoryProfile]]
}



case class MTBFileBundle
(
  identifier: Identifier,
  entry: MTBFileEntries
)
extends MTBFileBundleProfile

final case class MTBFileEntries
(
  patient:                EntryOf[MTBPatient],
  diagnoses:              NonEmptyList[EntryOf[Diagnosis]],
  previousGLTherapies:    List[EntryOf[PreviousGuidelineTherapy]],
//  cases: NonEmptyList[EntryOf[MTBCase]],
  lastGLTherapy:          EntryOf[LastGuidelineTherapy],
  ecogs:                  List[EntryOf[ObsECOG]],
  responses:              List[EntryOf[ObsRECIST]],
  specimens:              List[EntryOf[TumorSpecimen]],
  histology:              List[EntryOf[ObsHistology]],
  ngsReports:             List[EntryOf[SomaticNGSReport]],
  carePlans:              List[EntryOf[MTBCarePlan]],
  therapyRecommendations: List[EntryOf[TherapyRecommendation]],
  molecularTherapies:     List[EntryOf[MolecularTherapyHistory]]
)
extends MTBFileEntriesProfile



object MTBFileBundle
{

  import org.hl7.fhir.r4.json._
  import org.hl7.fhir.r4.json.backboneElements._

  implicit val mtbFileBundleProfile =
    Meta.Profiles[MTBFileBundle]("http://bwhc-mtb-file")
    
  implicit val formatMTBFileBundle =
    Json.format[MTBFileBundle]
  
}
