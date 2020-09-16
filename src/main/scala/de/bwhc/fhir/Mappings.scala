package de.bwhc.fhir



import java.time.temporal.Temporal
import java.time.LocalDate

import cats.data.NonEmptyList

import de.bwhc.util.mapping.syntax._
import de.bwhc.catalogs.icd
import de.bwhc.catalogs.icd._
import de.bwhc.catalogs.med
import de.bwhc.mtb.data.entry.dtos

import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Bundle.EntryOf

import CodingSystems._


object Mappings
{

  import scala.language.implicitConversions


  implicit class MapOps[K,V](val map: Map[K,V]) extends AnyVal
  {
    def invert: Map[V,K] =
      map.toSeq
       .foldLeft(List.empty[(V,K)])((l,kv) => (kv._2,kv._1) :: l)
       .toMap
  }


  implicit def openEndPeriodToFHIR[T <: Temporal]: dtos.OpenEndPeriod[T] => OpenEndPeriod[T] =
    p => OpenEndPeriod(p.start,p.end)


  implicit def openEndPeriodFromFHIR[T <: Temporal]: OpenEndPeriod[T] => dtos.OpenEndPeriod[T] =
    p => dtos.OpenEndPeriod(p.start,p.end)



  implicit def codingToFHIR[T: org.hl7.fhir.r4.Coding.System]: dtos.Coding[T] => BasicCoding[T] = 
    coding =>
      BasicCoding[T](
        coding.code.toString,
        coding.display,
        coding.version
      )



  //---------------------------------------------------------------------------
  // Gender mappings
  //---------------------------------------------------------------------------

  implicit val genderToAdminGender =
    Map[dtos.Gender.Value,AdministrativeGender.Value](
      dtos.Gender.Male    -> AdministrativeGender.Male,
      dtos.Gender.Female  -> AdministrativeGender.Female,
      dtos.Gender.Other   -> AdministrativeGender.Other,
      dtos.Gender.Unknown -> AdministrativeGender.Unknown
    )

  implicit val adminGenderToGender: AdministrativeGender.Value => dtos.Gender.Value =
    genderToAdminGender.invert



  //---------------------------------------------------------------------------
  // Organization Identifiers
  //---------------------------------------------------------------------------


  implicit def zpmToIdentifier(zpm: dtos.ZPM): Identifier =
    Identifier(zpm.value)

  implicit def healthInsuranceIdToIdentifier(ik: dtos.HealthInsurance.Id): Identifier =
    Identifier(ik.value)

  implicit def zpmFromIdentifier(id: Identifier): dtos.ZPM =
    dtos.ZPM(id.value)

  implicit def healthInsuranceIdFromIdentifier(id: Identifier): dtos.HealthInsurance.Id =
    dtos.HealthInsurance.Id(id.value)


  //---------------------------------------------------------------------------
  // Patient mappings
  //---------------------------------------------------------------------------


  implicit def patIdToIdentifier(id: dtos.Patient.Id) =
    Identifier(id.value)

  implicit def patIdFromIdentifier(id: Identifier) =
    dtos.Patient.Id(id.value)


  implicit val patientToFHIR: dtos.Patient => MTBPatient =
    pat =>
      MTBPatient(
        NonEmptyList.one(pat.id),
        pat.gender.mapTo[AdministrativeGender.Value],
        pat.birthDate,
        pat.dateOfDeath,
        pat.managingZPM.map(Reference[de.bwhc.fhir.ZPM](_)),
        pat.insurance
         .map(id => List(MTBPatient.HealthInsuranceContact(Reference[HealthInsurance](id))))
      )

  implicit val patientFromFHIR: MTBPatient => dtos.Patient =
    pat => 
      dtos.Patient(
        pat.identifier.head,
        pat.gender.mapTo[dtos.Gender.Value],
        pat.birthDate,
        pat.managingOrganization
          .flatMap(_.identifier)
          .map(id => dtos.ZPM(id.value)),
        pat.contact
          .flatMap(_.headOption)
          .flatMap(_.organization.identifier)
          .map(id => dtos.HealthInsurance.Id(id.value)),
        pat.deceasedDateTime
      )


  //---------------------------------------------------------------------------
  // MTBEpisode mappings
  //---------------------------------------------------------------------------


  implicit def episodeIdToIdentifier(id: dtos.MTBEpisode.Id): Identifier =
    Identifier(id.value)


  implicit val episodeToFHIR: dtos.MTBEpisode => MTBEpisode =
    eoc =>
      MTBEpisode(
        NonEmptyList.one(eoc.id),
        eoc.period.end
          .map(_ => EpisodeOfCare.Status.Finished)
          .getOrElse(EpisodeOfCare.Status.Active),
        Reference[MTBPatient](eoc.patient),
        eoc.period
      )

  implicit val episodeFromFHIR: MTBEpisode => dtos.MTBEpisode =
    eoc =>
      dtos.MTBEpisode(
        dtos.MTBEpisode.Id(eoc.identifier.head.value),
        dtos.Patient.Id(eoc.patient.identifier.get.value),
        eoc.period
      )

  
  //---------------------------------------------------------------------------
  // Consent mappings
  //---------------------------------------------------------------------------

  implicit def consentIdToIdentifier(id: dtos.Consent.Id): Identifier =
    Identifier(id.value)

  implicit def consentIdFromIdentifier(id: Identifier) =
    dtos.Consent.Id(id.value)

  implicit val consentStatusToFHIR =
    Map[dtos.Consent.Status.Value,Consent.Status.Value](
      dtos.Consent.Status.Active   -> Consent.Status.Active,
      dtos.Consent.Status.Rejected -> Consent.Status.Rejected,
    )
  
  implicit val consentStatusFromFHIR = consentStatusToFHIR.invert

  implicit val consentToFHIR: dtos.Consent => BwHCConsent =
    c =>
      BwHCConsent(
        NonEmptyList.one(c.id),
        c.status,
        Reference[MTBPatient](c.patient),
        BasicCodeableConcept(BasicCoding(Consent.Scope.Research)) 
      )

  implicit val consentFromFHIR: BwHCConsent => dtos.Consent =
    c =>
      dtos.Consent(
        c.identifier.head,
        c.patient.identifier.get,
        c.status
      )


  //---------------------------------------------------------------------------
  // Diagnosis mappings
  //---------------------------------------------------------------------------


  implicit def diagIdToIdentifier(id: dtos.Diagnosis.Id): Identifier =
    Identifier(id.value)

  implicit def diagIdFromIdentifier(id: Identifier) = dtos.Diagnosis.Id(id.value)

  implicit def histologyIdToIdentifier(id: dtos.HistologyResult.Id): Identifier =
    Identifier(id.value)

  implicit def histologyIdFromIdentifier(id: Identifier) = dtos.HistologyResult.Id(id.value)


  implicit val diagnosisToFHIR: dtos.Diagnosis => de.bwhc.fhir.Diagnosis = {
    diag =>
      de.bwhc.fhir.Diagnosis(
        NonEmptyList.one(diag.id),
        Reference[MTBPatient](diag.patient),
        diag.recordedOn,
        diag.icd10.map(icd => BasicCodeableConcept(icd.mapTo[BasicCoding[dtos.ICD10GM]])),
        diag.icdO3T.map(c =>
          List(
            BasicCodeableConcept(              
              BasicCoding[dtos.ICDO3T](
                c.code.value,
                c.display,
                Some(c.version.toString)
              )
            )

          )
        ),
        (
          diag.statusHistory.map(sts =>
            sts.map(st =>
              Diagnosis.Stage(
                Some(Tuple1(Diagnosis.Stage.Date(st.date))),
                BasicCodeableConcept( 
                  BasicCoding[dtos.Diagnosis.Status.Value](st.status.toString, None)
                )
              ) 
            )
          )
          .getOrElse(List.empty[Diagnosis.Stage[dtos.Diagnosis.Status.Value]]), 
          diag.whoGrade.map(who =>
            Diagnosis.Stage(
              diag.recordedOn.map(d => Tuple1(Diagnosis.Stage.Date(d))),
              BasicCodeableConcept(
                BasicCoding[dtos.WHOGrade.Value](who.code.toString,who.display)
              )
            )
          )
        ),
        diag.histologyResults.map(
          ids => ids.map(
            id => Diagnosis.HistologyEvidence(NonEmptyList.one(Reference[ObsHistology](id)))
          )
        )
      )  
  }


  implicit val diagnosisFromFHIR: Diagnosis => dtos.Diagnosis = {
    diag =>
       
      dtos.Diagnosis(
        diag.identifier.head,
        diag.subject.identifier.get,
        diag.recordedDate,
        diag.code
          .map(_.coding.head)
          .map(icd10 =>
            dtos.Coding(
              dtos.ICD10GM(icd10.code),
              icd10.display,
              icd10.version
            )
          ),
        diag.bodySite
          .flatMap(_.headOption)
          .map(_.coding.head)
          .map(icdO3t => 
            dtos.Coding( 
              dtos.ICDO3T(icdO3t.code),
              icdO3t.display,
              icdO3t.version
            )
          ),
        diag.stage._2
          .map(_.summary.coding.head)
          .map(who =>
            dtos.Coding(
              dtos.WHOGrade.withName(who.code),
              who.display
            )
          ),
        diag.evidence.map(
          _.flatMap(_.detail.toList.map(_.identifier.get))
        ),
        Option(
          diag.stage._1
            .map(st =>
              dtos.Diagnosis.StatusOnDate( 
                dtos.Diagnosis.Status.withName(st.summary.coding.head.code),
                st.extension.get._1.value
              )
            )
        )
        .filterNot(_.isEmpty)
      )

  }


  //---------------------------------------------------------------------------
  // Histology mappings
  //---------------------------------------------------------------------------

  implicit def specimenIdToIdentifier(id: dtos.Specimen.Id) =
    Identifier(id.value)

  implicit def specimenIdFromIdentifier(id: Identifier) =
    dtos.Specimen.Id(id.value)

  implicit val histologyResultToFHIR: dtos.HistologyResult => ObsHistology =
    histo =>
      ObsHistology(
        NonEmptyList.one(histo.id),
        Observation.Status.Final,
        histo.issuedOn,
        Reference[MTBPatient](histo.patient),
        Reference[Specimen](histo.specimen),
        histo.icdO3M.map(icdO3M =>
          BasicCodeableConcept(
            BasicCoding[dtos.ICDO3M](
              icdO3M.code.value,
              icdO3M.display,
              icdO3M.version
            )
          )
        ),
        histo.note.map(Protocol(_)).map(List(_))
      )


  implicit val histologyReportFromFHIR: ObsHistology => dtos.HistologyResult =
    obs =>
      dtos.HistologyResult(
        obs.identifier.head,
        obs.subject.identifier.get,
        obs.specimen.identifier.get,
        obs.effectiveDateTime,
        obs.valueCodeableConcept
          .map(_.coding.head)
          .map(coding =>
            dtos.Coding( 
              dtos.ICDO3M(coding.code),
              coding.display,
              coding.version
            )
          ),
        obs.note.flatMap(_.headOption).map(_.text)
      )


  //---------------------------------------------------------------------------
  // ECOG Performance Status mappings
  //---------------------------------------------------------------------------

  implicit def ecogIdFromIdentifier(id: Identifier) =
    dtos.ECOGStatus.Id(id.value)

  implicit def ecogIdToIdentifier(id: dtos.ECOGStatus.Id) =
    Identifier(id.value)


  implicit val ecogStatusToFHIR: dtos.ECOGStatus => ObsECOG = {
    ecog =>
      ObsECOG(
        NonEmptyList.one(ecog.id),
        Observation.Status.Final,
        ecog.effectiveDate,
        Reference[MTBPatient](ecog.patient),
        BasicCodeableConcept(
          BasicCoding[dtos.ECOG.Value](
            ecog.value.code.toString,
            ecog.value.display
          )
        ) 
      )
    }


  implicit val ecogStatusFromFHIR: ObsECOG => dtos.ECOGStatus =
    obs =>
      dtos.ECOGStatus(
        obs.identifier.head,
        obs.subject.identifier.get,
        obs.effectiveDateTime,
        dtos.Coding(
          dtos.ECOG.withName(obs.valueCodeableConcept.coding.head.code),
          obs.valueCodeableConcept.coding.head.display
        )
      )
  


  //---------------------------------------------------------------------------
  // Medication mappings
  //---------------------------------------------------------------------------

  import de.bwhc.mtb.data.entry.dtos.{Medication => ATC}


  implicit val medicationSetToFHIR: List[dtos.Coding[ATC]] => MTBMedication = {
    meds =>
      val id = meds.map(_.code.value).reduceLeft(_ + "-" + _)
      MTBMedication(
        id,
        NonEmptyList.fromListUnsafe(
          meds.map(m =>
            MTBMedication.Ingredient(
              BasicCodeableConcept(
                BasicCoding[ATC](
                  m.code.value,
                  m.display,
                )
              )
            )
          )
          .toList
        )
      )
  }

  implicit val medicationListFromFHIR: MTBMedication => List[dtos.Coding[ATC]] = {
    m =>
      m.ingredient
        .map(_.itemCodeableConcept.coding.head)
        .map(c =>
          dtos.Coding(
            dtos.Medication(c.code),
            c.display
          )
        )
        .toList
  }


/*
  //---------------------------------------------------------------------------
  // Guideline therapy mappings
  //---------------------------------------------------------------------------

  implicit val prevGLTherapyToFHIR: dtos.PreviousGuidelineTherapy => PreviousGuidelineTherapy = {

    th =>

      val medication = th.medication.mapTo[MTBMedication]

      PreviousGuidelineTherapy(
        NonEmptyList.one(th.id.mapTo[Identifier]),
        Tuple1(TherapyLine(PositiveInt(th.therapyLine.value))),
        Tuple1(medication),
        MedicationStatement.Status.Unknown,
        Reference[MTBPatient](th.patient.mapTo[Identifier]),
        Reference.contained(medication)
      )
  }

  implicit val prevGLTherapyFromFHIR: PreviousGuidelineTherapy => dtos.PreviousGuidelineTherapy = {
    th =>

      dtos.PreviousGuidelineTherapy(      
        dtos.GuidelineTherapy.Id(th.identifier.head.value),
        th.subject.identifier.map(_.value).map(dtos.Patient.Id).get,
        dtos.TherapyLine(th.modifierExtension._1.value.value),
        th.contained._1.mapTo[Set[med.Medication]]
      )

  }


  implicit val lastGLTherapyToFHIR:
    dtos.LastGuidelineTherapy => (LastGuidelineTherapy,ObsRECIST) = {

    th =>

      import LastGuidelineTherapy._
      import ObsRECIST._

      val med = th.medication.mapTo[MTBMedication]

      val glTh = LastGuidelineTherapy(
        NonEmptyList.one(th.id.mapTo[Identifier]),
        Tuple1(TherapyLine(PositiveInt(th.therapyLine.value))),
        Tuple1(med),
        MedicationStatement.Status.Stopped,
        NonEmptyList.one(BasicCodeableConcept(BasicCoding(th.reasonStopped,None))),
        Reference[MTBPatient](th.patient.mapTo[Identifier]),
        ClosedPeriod(th.period.start,th.period.end),
        Reference.contained(med)
      )

      val response =
        ObsRECIST(
          NonEmptyList.one(th.id.mapTo[Identifier]),  //TODO: generate random Identifier??
          Observation.Status.Final,
          NonEmptyList.one(Reference[LastGuidelineTherapy](glTh.identifier.head)),
       th.progressionDate,   //TODO: re-consider
          Reference[MTBPatient](th.patient.mapTo[Identifier]),
          BasicCodeableConcept(BasicCoding(th.response.code, th.response.display))
        )

      (glTh,response)
  }

  implicit val lastGLTherapyFromFHIR:
   ( (LastGuidelineTherapy,ObsRECIST) )=> dtos.LastGuidelineTherapy = {

    case (th,response) =>

      dtos.LastGuidelineTherapy(
        dtos.GuidelineTherapy.Id(th.identifier.head.value),
        th.subject.identifier.map(_.value).map(dtos.Patient.Id).get,
        dtos.TherapyLine(th.modifierExtension._1.value.value),
        dtos.ClosedPeriod(
          th.effectivePeriod.start,th.effectivePeriod.end
        ),
        th.contained._1.mapTo[Set[med.Medication]],
        dtos.RECISTCoding(
          dtos.RECIST.withName(response.valueCodeableConcept.coding.head.code)
        ),
        None, //TODO: re-consider
        dtos.GuidelineTherapy.StopReason.withName(th.statusReason.head.coding.head.code)
      )

  }


  //---------------------------------------------------------------------------
  // Specimen mappings
  //---------------------------------------------------------------------------

  implicit val specimenToFHIR: dtos.Specimen => TumorSpecimen = {
    sp =>

      import TumorSpecimen._

      TumorSpecimen(
        NonEmptyList.one(sp.id.mapTo[Identifier]),
        NonEmptyList.one(TumorSpecimen.Diagnosis(BasicCodeableConcept(sp.icd10.mapTo[BasicCoding[ICD10GM]]))),
        Reference[MTBPatient](sp.patient.mapTo[Identifier]),
        TumorSpecimen.Collection(
          sp.collectionDate,
          BasicCodeableConcept(BasicCoding(sp.localization,None)),
          BasicCodeableConcept(BasicCoding(sp.collectionMethod,None))
        ),
        sp.`type`.map(t => List(BasicCodeableConcept(BasicCoding(t.toString,None))))
      )
  }

  implicit val specimenFromFHIR: TumorSpecimen => dtos.Specimen = {
    sp =>

      val icd10 = sp.modifierExtension.head.value.coding.head

      dtos.Specimen(
        dtos.Specimen.Id(sp.identifier.head.value),
        sp.subject.identifier.map(_.value).map(dtos.Patient.Id).get,
        ICD10GMCoding(
          icd.ICD10GM.Code(icd10.code),
          icd10.display,
          icd10.version.map(icd.ICD10GM.Version(_)).get
        ), 
        dtos.Specimen.Localization.withName(sp.collection.bodySite.coding.head.code),
        sp.condition.flatMap(_.headOption)
          .map(_.coding.head.code)
          .map(dtos.Specimen.Type.withName _),
        sp.collection.collectedDateTime,
        dtos.Specimen.CollectionMethod.withName(sp.collection.method.coding.head.code)        
      )

  }

  //---------------------------------------------------------------------------
  // Somatic NGS Report mappings
  //---------------------------------------------------------------------------

  import java.util.UUID.{randomUUID => rndID}

  def toFHIR(
    variant: dtos.SimpleVariant
  )(
    implicit subject: Reference[MTBPatient]
  ): SimpleVariant = {

    import ObsVariant._
    import SimpleVariant._
      
      SimpleVariant(
        rndID.toString,
        NonEmptyList.one(variant.cosmicId.mapTo[Identifier]),
        Observation.Status.Final,
        subject,
        SimpleVariant.Components(
          NonEmptyList.one(GeneStudied(BasicCodeableConcept(BasicCoding[HGNC](variant.gene.value,None)))),
//          ExactStartEnd(LBoundedRange(variant.startEnd.start.toDouble,Some(variant.startEnd.end.toDouble))),
          ExactStartEnd(variant.startEnd.toString),
          RefAllele(variant.refAllele.value),
          AltAllele(variant.altAllele.value),
          AminoAcidChange(BasicCodeableConcept(BasicCoding[HGVS](variant.aminoAcidChange.value,None))),
          DNAChange(BasicCodeableConcept(BasicCoding[HGVS](variant.dnaChange.value,None))),
          DbSNPId(BasicCodeableConcept(BasicCoding[dbSNP](variant.dbSNPId.value,None))),
          SampleAllelicFrequency(SimpleQuantity(variant.allelicFrequency.value)),
          AllelicReadDepth(SimpleQuantity(variant.readDepth.value))
        ),
        NonEmptyList.one(
          BasicCodeableConcept(BasicCoding[ClinVar](variant.interpretation.value,None)),
        )
      )
  }  


  implicit val variantFromFHIR: SimpleVariant => dtos.SimpleVariant = {
    variant =>

      import de.bwhc.catalogs.hgnc.HGNCGene
      import dtos.Variant._
      import dtos.SimpleVariant._

      val SimpleVariant.Components(
        geneStudied,
        exactStartEnd,
        refAllele,
        altAllele,
        aminoAcidChange,
        dnaChange,
        dbSNPId,
        sampleAllelicFrequency,
        allelicReadDepth
      ) = variant.component


      dtos.SimpleVariant(
        HGNCGene.Symbol(geneStudied.head.valueCodeableConcept.coding.head.code),
        StartEnd.parse(exactStartEnd.valueString),
        Allele(refAllele.valueString),
        Allele(altAllele.valueString),
        DNAChange(aminoAcidChange.valueCodeableConcept.coding.head.code),
        AminoAcidChange(aminoAcidChange.valueCodeableConcept.coding.head.code),
        AllelicReadDepth(allelicReadDepth.valueQuantity.value.toInt),
        AllelicFrequency(sampleAllelicFrequency.valueQuantity.value),
        CosmicId(variant.identifier.head.value),
        DbSNPId(dbSNPId.valueCodeableConcept.coding.head.code),
        Interpretation(variant.interpretation.head.coding.head.code)
      )
  }

  def toFHIR(
    tc: dtos.TumorContent
  )(
    implicit
    subject: Reference[MTBPatient],
    specimen: Reference[TumorSpecimen]
  ): ObsTumorContent = {

      import ObsTumorContent._

      ObsTumorContent(
        rndID.toString,
        Observation.Status.Final,
        subject,
        specimen,
        BasicCodeableConcept(BasicCoding(tc.method, None)),
        SimpleQuantity(tc.value)          
      )
  }

  implicit val tumorContentFromFHIR: ObsTumorContent => dtos.TumorContent = {
    tc =>
      dtos.TumorContent(
        dtos.TumorContent.Method.withName(tc.method.coding.head.code),
        tc.specimen.identifier.map(_.value).map(dtos.Specimen.Id).get,
        tc.valueQuantity.value
      ) 
  }

  implicit val ngsReportToFHIR: dtos.SomaticNGSReport => SomaticNGSReport = {
    ngs =>
  
      implicit val subject  = Reference[MTBPatient](ngs.patient.mapTo[Identifier])
      implicit val specimen = Reference[TumorSpecimen](ngs.specimen.mapTo[Identifier])

      val tumorContent =
        ngs.tumorContent.map(toFHIR)

      val tmb =
        ObsTMB(
          rndID.toString,
          Observation.Status.Final,
          subject,
          specimen,
          SimpleQuantity(ngs.tmb.value,Some("mut/Mb"))          
        )

      val msi =
        ObsMSI(
          rndID.toString,
          Observation.Status.Final,
          subject,
          specimen,
          SimpleQuantity(ngs.msi.value)          
        )

      val brcaness =
        ObsBRCAness(
          rndID.toString,
          Observation.Status.Final,
          subject,
          specimen,
          SimpleQuantity(ngs.brcaness.value)          
        )

      val simpleVariants = ngs.simpleVariants.map(toFHIR(_)).toList

      SomaticNGSReport(
        NonEmptyList.one(ngs.id.mapTo[Identifier]),
        ngs.issueDate,
        DiagnosticReport.Status.Final,
        subject,
        NonEmptyList.one(specimen),
        NonEmptyList.of(
          Reference.contained(tmb),
          Reference.contained(msi),
          Reference.contained(brcaness)
        ) ++ tumorContent.map(Reference.contained(_))
          ++ simpleVariants.map(Reference.contained(_)),
        (
          tumorContent,
          tmb,
          msi,
          brcaness,
          simpleVariants
        )
      )

  }

  implicit val ngsReportFromFHIR: SomaticNGSReport => dtos.SomaticNGSReport = {
    ngs =>

      import dtos.SomaticNGSReport._

      val (tumorContent,tmb,msi,brcaness,simpleVariants) = ngs.contained

      dtos.SomaticNGSReport(  
        Id(ngs.identifier.head.value),
        ngs.subject.identifier.map(_.value).map(dtos.Patient.Id).get,
        ngs.specimen.head.identifier.map(_.value).map(dtos.Specimen.Id).get,
        ngs.issued,
//        ???,  //TODO TODO
        tumorContent.map(_.mapTo[dtos.TumorContent]),
        BRCAness(brcaness.valueQuantity.value),
        MSI(msi.valueQuantity.value),
        TMB(tmb.valueQuantity.value),
//        ???,  //TODO TODO
        simpleVariants.map(_.mapTo[SimpleVariant])
      )
  }


  //---------------------------------------------------------------------------
  // CarePlan / TherapyRecommendation mappings
  //---------------------------------------------------------------------------
  
  implicit val priorityToFHIR =
    Map[dtos.TherapyRecommendation.Priority.Value, MedicationRequest.Priority.Value](
      dtos.TherapyRecommendation.Priority.One   -> MedicationRequest.Priority.Stat,
      dtos.TherapyRecommendation.Priority.Two   -> MedicationRequest.Priority.Asap,
      dtos.TherapyRecommendation.Priority.Three -> MedicationRequest.Priority.Urgent,
      dtos.TherapyRecommendation.Priority.Four  -> MedicationRequest.Priority.Routine
    )

  implicit val priorityFromFHIR:
    Map[MedicationRequest.Priority.Value, dtos.TherapyRecommendation.Priority.Value] =
      priorityToFHIR.invert


  implicit val therapyRecommendationToFHIR:
    dtos.TherapyRecommendation => TherapyRecommendation = {

      rec => 

        import LoE._

        val med = rec.medication.mapTo[MTBMedication]

        TherapyRecommendation(
          NonEmptyList.one(rec.id.mapTo[Identifier]),
          Tuple1(
            LoE(
              LoE.Grade(BasicCoding(rec.levelOfEvidence.grading,None)),
              rec.levelOfEvidence
                .addendums
                .map(add => BasicCoding(add,None))
                .map(LoE.Addendum(_))
            )
          ),
          Tuple1(med),
          rec.priority.mapTo[MedicationRequest.Priority.Value],
          MedicationRequest.Status.Unknown,          
          MedicationRequest.Intent.Proposal,
          rec.issueDate,
          Reference[MTBPatient](rec.patient.mapTo[Identifier]),
          Reference.contained(med),
          NonEmptyList.one(
            Reference[SomaticVariantProfile](rec.supportingVariant.mapTo[Identifier]) //TODO: re-consider
          )  
        )

  }

  implicit val therapyRecommendationFromFHIR:
    TherapyRecommendation => dtos.TherapyRecommendation = {

      rec =>
 
        val loe = rec.modifierExtension._1

        dtos.TherapyRecommendation(       
          dtos.TherapyRecommendation.Id(rec.identifier.head.value),        
          rec.authoredOn,
          rec.subject.identifier.map(_.value).map(dtos.Patient.Id).get,
          dtos.LevelOfEvidence(
            dtos.LevelOfEvidence.Grading.withName(loe.grade.value.code),
            loe.addendums.map(_.value.code).map(dtos.LevelOfEvidence.Addendum.withName _)
          ),
          rec.priority.mapTo[dtos.TherapyRecommendation.Priority.Value],
          dtos.Variant.CosmicId(rec.supportingInformation.head.identifier.get.value),
          rec.contained._1.mapTo[Set[med.Medication]]
        )
    }


  type CarePlanWithRecommendations = (MTBCarePlan,NonEmptyList[TherapyRecommendation])

  implicit val carePlanToFHIR: dtos.CarePlan => CarePlanWithRecommendations = {

    cp => 

      val recs = cp.recommendations.map(_.mapTo[TherapyRecommendation])

      val carePlan =
        MTBCarePlan(        
          NonEmptyList.one(cp.id.mapTo[Identifier]),
          CarePlan.Status.Unknown,
          CarePlan.Intent.Proposal,
          cp.issuedOn,
          Reference[MTBPatient](cp.patient.mapTo[Identifier]),
          cp.description,
          recs.map(_.identifier.head)
              .map(Reference[TherapyRecommendation](_))
              .map(MTBCarePlan.Activity)
        )

      (carePlan,recs)
  }

  implicit val carePlanFromFHIR: CarePlanWithRecommendations => dtos.CarePlan = {

    case (cp,recs) => 

      dtos.CarePlan(
        dtos.CarePlan.Id(cp.identifier.head.value),
        cp.subject.identifier.map(_.value).map(dtos.Patient.Id).get,
        cp.created,
        cp.description,
        recs.map(_.mapTo[dtos.TherapyRecommendation]),
        None //TODO TODO
      )

  }

  //---------------------------------------------------------------------------
  // Molecular Therapy mappings
  //---------------------------------------------------------------------------

  implicit val dosageToFhir: dtos.Dosage.Value => DosageDensity = {
    case dtos.Dosage.Under50Percent => DosageDensity(DosageRange(BasicRange(0,50)))
    case dtos.Dosage.Over50Percent  => DosageDensity(DosageRange(BasicRange(50,100)))
  }

  implicit val dosageFromFhir: DosageDensity => dtos.Dosage.Value = {
    dosage =>
      if (dosage.doseAndRate.doseRange.contains(25)) dtos.Dosage.Under50Percent
      else dtos.Dosage.Over50Percent

  }


  implicit val molecularTherapyToFHIR: dtos.MolecularTherapy => MolecularTherapy = {

    import MolecularTherapy.Systems._

    molTh =>

      val identifier = NonEmptyList.one(molTh.id.mapTo[Identifier])
      val subject    = Reference[MTBPatient](molTh.patient.mapTo[Identifier])
      val basedOn    = NonEmptyList.one(Reference[TherapyRecommendation](molTh.basedOn.mapTo[Identifier]))
      val note       = NonEmptyList.one(Note(molTh.note))

      molTh match {

        case th: dtos.NotDoneTherapy => {
          NotTakenMolecularTherapy(
            identifier,
            basedOn,
            molTh.recordedOn,
            subject,
            Reference[MTBMedication]("DUMMY"),
            NonEmptyList.one(
              BasicCodeableConcept(BasicCoding(th.notDoneReason.toString,None))
            ),
            note
          )
        }

        case th: dtos.StoppedTherapy => {

          val medication = th.medication.mapTo[MTBMedication]

          StoppedMolecularTherapy(
            identifier,
            Tuple1(medication),
            basedOn,
            molTh.recordedOn,
            subject,
            Reference.contained(medication),
            ClosedPeriod(th.period.start,th.period.end),
            th.dosage.map(_.mapTo[DosageDensity]).map(List(_)),
            NonEmptyList.one(
              BasicCodeableConcept(BasicCoding(th.reasonStopped.toString,None))
            ),
            note
          )
        }

        case th: dtos.CompletedTherapy => {

          val medication = th.medication.mapTo[MTBMedication]

          CompletedMolecularTherapy(
            identifier,
            Tuple1(medication),
            basedOn,
            molTh.recordedOn,
            subject,
            Reference.contained(medication),
            ClosedPeriod(th.period.start,th.period.end),
            th.dosage.map(_.mapTo[DosageDensity]).map(List(_)),
            note
          )
        }

        case th: dtos.OngoingTherapy => {

          val medication = th.medication.mapTo[MTBMedication]

          ActiveMolecularTherapy(
            identifier,
            Tuple1(medication),
            basedOn,
            molTh.recordedOn,
            subject,
            Reference.contained(medication),
            OpenEndPeriod(th.period.start),
            th.dosage.map(_.mapTo[DosageDensity]).map(List(_)),
            note
          )
        }

      }
  }

  implicit val molTherapyDocToFHIR:
    dtos.MolecularTherapyDocumentation => MolecularTherapyHistory = {

      doc =>

        MolecularTherapyHistory(
          Bundle.History.Entries(
            doc.history.map(_.mapTo[MolecularTherapy]).map(EntryOf(_))
          )
        )

    }


  implicit val molecularTherapyFromFHIR: MolecularTherapy => dtos.MolecularTherapy = {

    import dtos.MolecularTherapy._

    molTh =>

      val id      = dtos.MolecularTherapy.Id(molTh.identifier.head.value)
      val patient = dtos.Patient.Id(molTh.subject.identifier.get.value) 
      val basedOn = dtos.TherapyRecommendation.Id(molTh.basedOn.head.identifier.get.value)
      val note    = molTh.note.head.text

      molTh match {

        case th: NotTakenMolecularTherapy => {
          dtos.NotDoneTherapy(
            id,
            patient,
            th.dateAsserted,
            basedOn,
            NotDoneReason.withName(th.statusReason.head.coding.head.code),
            note
          )
        }

        case th: StoppedMolecularTherapy => {
          dtos.StoppedTherapy(
            id,
            patient,
            molTh.dateAsserted,
            basedOn,
            note,
            dtos.ClosedPeriod(th.effectivePeriod.start,th.effectivePeriod.end),
            th.contained._1.mapTo[Set[med.Medication]],
            th.dosage.flatMap(_.headOption).map(_.mapTo[dtos.Dosage.Value]),
            StopReason.withName(th.statusReason.head.coding.head.code)
          )
        }

        case th: CompletedMolecularTherapy => {
          dtos.CompletedTherapy(
            id,
            patient,
            molTh.dateAsserted,
            basedOn,
            note,
            dtos.ClosedPeriod(th.effectivePeriod.start,th.effectivePeriod.end),
            th.contained._1.mapTo[Set[med.Medication]],
            th.dosage.flatMap(_.headOption).map(_.mapTo[dtos.Dosage.Value])
          )
        }

        case th: ActiveMolecularTherapy => {
          dtos.OngoingTherapy(
            id,
            patient,
            molTh.dateAsserted,
            basedOn,
            note,
            dtos.OpenEndPeriod(th.effectivePeriod.start),
            th.contained._1.mapTo[Set[med.Medication]],
            th.dosage.flatMap(_.headOption).map(_.mapTo[dtos.Dosage.Value])
          )
        }

      }
  }

  implicit val molTherapyDocFromFHIR:
    MolecularTherapyHistory => dtos.MolecularTherapyDocumentation = {

      doc =>

        val Bundle.History.Entries(entries) = doc.entry

        dtos.MolecularTherapyDocumentation(
          entries.map(_.resource).map(_.mapTo[dtos.MolecularTherapy])
        )

    }



  //---------------------------------------------------------------------------
  // MTB File mappings
  //---------------------------------------------------------------------------

  implicit val mtbFileToFHIR: dtos.MTBFile => MTBFileBundle = {

    mtbfile =>

      val pat      = mtbfile.patient.mapTo[MTBPatient]
      
      val (lastGL,response) =
        mtbfile.lastGuidelineTherapy.mapTo[(LastGuidelineTherapy,ObsRECIST)]
      
      val (carePlans,recs) =
        mtbfile.carePlans.map(_.mapTo[CarePlanWithRecommendations]).unzip
      
      MTBFileBundle(
        Identifier(rndID.toString),
        MTBFileEntries(
          EntryOf(pat),
          mtbfile.diagnoses.map(_.mapTo[Diagnosis]).map(EntryOf(_)),
          mtbfile.guidelineTherapies.map(_.mapTo[PreviousGuidelineTherapy]).map(EntryOf(_)),
//       NonEmptyList.of(EntryOf(mtbcase)),    
          EntryOf(lastGL),
          mtbfile.ecogStatus.map(_.mapTo[ObsECOG]).map(EntryOf(_)),
      List(EntryOf(response)),
          mtbfile.specimens.map(_.mapTo[TumorSpecimen]).map(EntryOf(_)),
          mtbfile.histologyReports.map(_.mapTo[ObsHistology]).map(EntryOf(_)),
          mtbfile.ngsReports.map(_.mapTo[SomaticNGSReport]).map(EntryOf(_)),
          carePlans.map(EntryOf(_)),
          recs.flatMap(rs => rs.toList).map(EntryOf(_)),
          mtbfile.molecularTherapies.map(_.mapTo[MolecularTherapyHistory]).map(EntryOf(_))
//TODO: mol. th. responses
        )
      )

    }


  implicit val mtbFileFromFHIR: MTBFileBundle => dtos.MTBFile = {

    mtbfile =>

      val MTBFileEntries(
        patient, 
        diagnoses,
        previousGLTherapies,
        lastGLTherapy,
        ecogs,
        responses,
        specimens,
        histology,
        ngsReports,
        carePlans,
        therapyRecommendations,
        molecularTherapies
      ) = mtbfile.entry


      val EntryOf(lastGL) = lastGLTherapy
      val EntryOf(lastGLResponse) = responses.find(_.resource.partOf.head.identifier.get == lastGL.identifier.head).get

      val recommendations = therapyRecommendations.map(_.resource)
      val cpsWithRecs = 
        for {
          cp      <- carePlans.map(_.resource)
          recs    =  cp.activity.map(act => recommendations.find(_.identifier.head == act.reference.identifier.get).get)
        } yield (cp,recs)


      dtos.MTBFile(
        patient.resource.mapTo[dtos.Patient],
   dtos.OpenEndPeriod(java.time.LocalDate.now),  //TODO TODO
        diagnoses.map(_.resource.mapTo[dtos.Diagnosis]),
        previousGLTherapies.map(_.resource.mapTo[dtos.PreviousGuidelineTherapy]),
        (lastGL,lastGLResponse).mapTo[dtos.LastGuidelineTherapy],
        ecogs.map(_.resource.mapTo[dtos.ECOGStatus]),         
        specimens.map(_.resource.mapTo[dtos.Specimen]),
        histology.map(_.resource.mapTo[dtos.HistologyReport]),
        ngsReports.map(_.resource.mapTo[dtos.SomaticNGSReport]),
        cpsWithRecs.map(_.mapTo[dtos.CarePlan]),
        molecularTherapies.map(_.resource.mapTo[dtos.MolecularTherapyDocumentation]),
        List.empty[dtos.FollowUp]
      )


  }

*/

}
