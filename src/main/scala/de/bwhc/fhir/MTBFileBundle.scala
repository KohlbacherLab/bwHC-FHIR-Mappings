package de.bwhc.fhir


import java.time.LocalDate

import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Bundle.{
  Entry, EntryElement, EntryOf
}

import play.api.libs.json.Json

import cats.data.NonEmptyList



trait MTBFileBundleProfile
extends Bundle.Collection
   with Bundle.entry[MTBFileEntriesProfile]
  

trait MTBFileEntriesProfile
extends Bundle.EntrySet
{
  this: Product =>

  val patient:                EntryElement with Entry.resource[MTBPatientProfile]
  val episode:                EntryElement with Entry.resource[MTBEpisodeProfile]  
  val consent:                EntryElement with Entry.resource[BwHCConsentProfile]  
  val diagnoses:              List[EntryElement with Entry.resource[DiagnosisProfile]]
  val familyMemberDiagnoses:  List[EntryElement with Entry.resource[FamilyMemberHistoryProfile]]
  val previousGLTherapies:    List[EntryElement with Entry.resource[GuidelineTherapyProfile]]
  val lastGLTherapy:          Option[EntryElement with Entry.resource[LastGuidelineTherapyProfile]]
  val ecogs:                  List[EntryElement with Entry.resource[ObsECOGProfile]]
  val specimens:              List[EntryElement with Entry.resource[TumorSpecimenProfile]]
//  val histology:              List[EntryElement with Entry.resource[ObsHistologyProfile]]
  val ngsReports:             List[EntryElement with Entry.resource[SomaticNGSReportProfile]]
  val carePlans:              List[EntryElement with Entry.resource[MTBCarePlanProfile]]
  val therapyRecommendations: List[EntryElement with Entry.resource[TherapyRecommendationProfile]]
  val molecularTherapies:     List[EntryElement with Entry.resource[MolecularTherapyHistoryProfile]]
  val responses:              List[EntryElement with Entry.resource[ObsRECISTProfile]]
}



case class MTBFileBundle
(
  entry: MTBFileEntries
)
extends MTBFileBundleProfile

final case class MTBFileEntries
(
  patient:                EntryOf[MTBPatient],
  episode:                EntryOf[MTBEpisode],
  consent:                EntryOf[BwHCConsent],
  diagnoses:              List[EntryOf[Diagnosis]],
  familyMemberDiagnoses:  List[EntryOf[FamilyMemberHistoryDTO]],
  previousGLTherapies:    List[EntryOf[PreviousGuidelineTherapy]],
  lastGLTherapy:          Option[EntryOf[LastGuidelineTherapy]],
  ecogs:                  List[EntryOf[ObsECOG]],
  specimens:              List[EntryOf[TumorSpecimen]],
//  histology:              List[EntryOf[ObsHistology]],
  ngsReports:             List[EntryOf[SomaticNGSReport]],
  carePlans:              List[EntryOf[MTBCarePlan]],
  therapyRecommendations: List[EntryOf[TherapyRecommendation]],
  molecularTherapies:     List[EntryOf[MolecularTherapyHistory]],
  responses:              List[EntryOf[ObsRECIST]]
)
extends MTBFileEntriesProfile



object MTBFileBundle
{

  import org.hl7.fhir.r4.json._
  import org.hl7.fhir.r4.json.backboneElements._

  implicit val mtbFileBundleProfile =
    Meta.Profiles[MTBFileBundle]("http://bwhc.de/mtb-file")
    
  implicit val formatMTBFileBundle =
    Json.format[MTBFileBundle]
  
}
