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
   with Bundle.entry[
     Bundle.EntrySet {
       val patient:                    EntryElement with Entry.resource[MTBPatientProfile]
       val episode:                    EntryElement with Entry.resource[MTBEpisodeProfile]  
       val consent:                    EntryElement with Entry.resource[BwHCConsentProfile]  
       val diagnoses:                  List[EntryElement with Entry.resource[DiagnosisProfile]]
       val familyMemberDiagnoses:      List[EntryElement with Entry.resource[FamilyMemberHistoryProfile]]
       val previousGLTherapies:        List[EntryElement with Entry.resource[GuidelineTherapyProfile]]
       val lastGLTherapies:            List[EntryElement with Entry.resource[LastGuidelineTherapyProfile]]
       val ecogs:                      List[EntryElement with Entry.resource[ObsECOGProfile]]
       val specimens:                  List[EntryElement with Entry.resource[TumorSpecimenProfile]]
       val molecularPathology:         List[EntryElement with Entry.resource[MolecularPathologyReport]]
       val histology:                  List[EntryElement with Entry.resource[HistologyReportProfile]]
       val ngsReports:                 List[EntryElement with Entry.resource[SomaticNGSReportProfile]]
       val carePlans:                  List[EntryElement with Entry.resource[MTBCarePlanProfile]]
       val therapyRecommendations:     List[EntryElement with Entry.resource[TherapyRecommendationProfile]]
       val geneticCounsellingRequests: List[EntryElement with Entry.resource[CounsellingRequestProfile]]
       val rebiopsyRequests:           List[EntryElement with Entry.resource[RebiopsyRequestProfile]]
//       val histologyReevaluationRequests: List[EntryElement with Entry.resource[HistologyReevaluationRequestProfile]]
//TODO: studyInclusion
       val claims:                     List[EntryElement with Entry.resource[ClaimProfile]]
       val claimResponses:             List[EntryElement with Entry.resource[ClaimResponseProfile]]
       val molecularTherapies:         List[EntryElement with Entry.resource[MolecularTherapyHistoryProfile]]
       val responses:                  List[EntryElement with Entry.resource[ObsRECISTProfile]]
     }
   ]



case class MTBFileBundle
(
  entry: MTBFileBundle.Entries
)
extends MTBFileBundleProfile


object MTBFileBundle
{

  import org.hl7.fhir.r4.json._
  import org.hl7.fhir.r4.json.backboneElements._


  final case class Entries
  (
    patient:                    EntryOf[MTBPatient],
    episode:                    EntryOf[MTBEpisode],
    consent:                    EntryOf[BwHCConsent],
    diagnoses:                  List[EntryOf[Diagnosis]],
    familyMemberDiagnoses:      List[EntryOf[FamilyMemberHistoryDTO]],
    previousGLTherapies:        List[EntryOf[PreviousGuidelineTherapy]],
    lastGLTherapies:            List[EntryOf[LastGuidelineTherapy]],
    ecogs:                      List[EntryOf[ObsECOG]],
    specimens:                  List[EntryOf[TumorSpecimen]],
    molecularPathology:         List[EntryOf[MolecularPathologyReport]],
    histology:                  List[EntryOf[HistologyReport]],
    ngsReports:                 List[EntryOf[SomaticNGSReport]],
    carePlans:                  List[EntryOf[MTBCarePlan]],
    therapyRecommendations:     List[EntryOf[TherapyRecommendation]],
    geneticCounsellingRequests: List[EntryOf[CounsellingRequest]],
    rebiopsyRequests:           List[EntryOf[RebiopsyRequest]],
//    histologyReevaluationRequests: List[EntryOf[HistologyReevaluationRequest]],
    claims:                     List[EntryOf[ClaimDTO]],
    claimResponses:             List[EntryOf[ClaimResponseDTO]],
    molecularTherapies:         List[EntryOf[MolecularTherapyHistory]],
    responses:                  List[EntryOf[ObsRECIST]]
  )
  extends Bundle.EntrySet


  implicit val mtbFileBundleProfile =
    Meta.Profiles[MTBFileBundle]("http://bwhc.de/mtb-file")
    
  implicit val formatMTBFileBundle =
    Json.format[MTBFileBundle]
  
}
