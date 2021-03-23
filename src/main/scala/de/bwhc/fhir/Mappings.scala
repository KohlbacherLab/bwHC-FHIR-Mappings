package de.bwhc.fhir



import java.time.temporal.Temporal
import java.time.LocalDate
import java.util.UUID.{randomUUID => rndID}

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


  implicit def mapBundleEntry[R <: Resource,T](
    implicit f: R => T
  ): Bundle.EntryElement with Bundle.Entry.resource[R] => T = {
    entry => entry.resource.mapTo[T]
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


  implicit val icd10CodingToFHIR: dtos.Coding[dtos.ICD10GM] => BasicCoding[dtos.ICD10GM] = 
    coding =>
      BasicCoding[dtos.ICD10GM](
        coding.code.value,
        coding.display,
        coding.version
      )

  implicit val icd10CodingFromFHIR: BasicCoding[dtos.ICD10GM] => dtos.Coding[dtos.ICD10GM] = 
    coding =>
      dtos.Coding[dtos.ICD10GM](
        dtos.ICD10GM(coding.code),
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
         .map(id => List(MTBPatient.HealthInsuranceContact(LogicalReference[HealthInsurance](id))))
      )

  implicit val patientFromFHIR: MTBPatient => dtos.Patient =
    pat => 
      dtos.Patient(
        pat.identifier.head,
        pat.gender.mapTo[dtos.Gender.Value],
        pat.birthDate,
        pat.managingOrganization
          .map(_.identifier)
          .map(id => dtos.ZPM(id.value)),
        pat.contact
          .flatMap(_.headOption)
          .map(_.organization.identifier)
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
        LogicalReference[MTBPatient](eoc.patient),
        eoc.period
      )

  implicit val episodeFromFHIR: MTBEpisode => dtos.MTBEpisode =
    eoc =>
      dtos.MTBEpisode(
        dtos.MTBEpisode.Id(eoc.identifier.head.value),
        dtos.Patient.Id(eoc.patient.identifier.value),
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
        c.patient.identifier,
        c.status
      )


  //---------------------------------------------------------------------------
  // Diagnosis mappings
  //---------------------------------------------------------------------------


  implicit def diagIdToIdentifier(id: dtos.Diagnosis.Id): Identifier =
    Identifier(id.value)

  implicit def diagIdFromIdentifier(id: Identifier) = dtos.Diagnosis.Id(id.value)


  implicit def histologyIdToIdentifier(id: dtos.HistologyReport.Id): Identifier =
    Identifier(id.value)

  implicit def histologyIdFromIdentifier(id: Identifier) = dtos.HistologyReport.Id(id.value)


  implicit val diagnosisToFHIR: dtos.Diagnosis => de.bwhc.fhir.Diagnosis = {
    diag =>
      de.bwhc.fhir.Diagnosis(
        NonEmptyList.one(diag.id),
        LogicalReference[MTBPatient](diag.patient),
        diag.recordedOn,
        diag.icd10.map(icd => BasicCodeableConcept(icd.mapTo[BasicCoding[dtos.ICD10GM]])),
        diag.icdO3T.map(c =>
          List(
            BasicCodeableConcept(              
              BasicCoding[dtos.ICDO3T](
                c.code.value,
                c.display,
                c.version
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
        diag.subject.identifier,
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
          _.flatMap(_.detail.toList.map(_.identifier))
        ),
        Option(
          diag.stage._1
            .map(st =>
              dtos.Diagnosis.StatusOnDate( 
                dtos.Diagnosis.Status.withName(st.summary.coding.head.code),
                st.extension.get._1.value
              )
            )
        ).filterNot(_.isEmpty),
  None  //TODO: Model guideline treatment status
      )

  }


  //---------------------------------------------------------------------------
  // Family Member Diagnosis mappings
  //---------------------------------------------------------------------------

  implicit def familyMemberDiagIdToIdentifier(id: dtos.FamilyMemberDiagnosis.Id) =
    Identifier(id.value)

  implicit def familyMemberDiagIdFromIdentifier(id: Identifier) =
    dtos.FamilyMemberDiagnosis.Id(id.value)

  implicit val familyMemberRelationshipToHL7v3 =
    Map[dtos.FamilyMember.Relationship.Value,HL7v3FamilyMember.Value](
      dtos.FamilyMember.Relationship.FamilyMember         -> HL7v3FamilyMember.FAMMEMB ,
      dtos.FamilyMember.Relationship.ExtendedFamilyMember -> HL7v3FamilyMember.EXT     
    )

  implicit val familyMemberRelationshipFromHL7v3 =
    familyMemberRelationshipToHL7v3.invert


  
  implicit val familyMemberDiagnosisToFHIR: 
    dtos.FamilyMemberDiagnosis => FamilyMemberHistoryDTO =
      diag =>
        FamilyMemberHistoryDTO(
          NonEmptyList.one(diag.id),
          FamilyMemberHistory.Status.HealthUnknown,
          LogicalReference[MTBPatient](diag.patient),
          BasicCodeableConcept(BasicCoding(diag.relationship.code))
        )

  implicit val familyMemberDiagnosisFromFHIR: 
    FamilyMemberHistoryDTO => dtos.FamilyMemberDiagnosis =
      fmh =>
        dtos.FamilyMemberDiagnosis(
          fmh.identifier.head,
          fmh.patient.identifier,
          dtos.Coding(
            HL7v3FamilyMember.withName(fmh.relationship.coding.head.code).mapTo[dtos.FamilyMember.Relationship.Value],
            None
          )
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

      import de.bwhc.mtb.data.entry.dtos.ValueSets._

      ObsECOG(
        NonEmptyList.one(ecog.id),
        Observation.Status.Final,
        ecog.effectiveDate,
        LogicalReference[MTBPatient](ecog.patient),
        BasicCodeableConcept(
          BasicCoding[dtos.ECOG.Value](
            ecog.value.code.toString,
            dtos.ValueSet[dtos.ECOG.Value].displayOf(ecog.value.code),
//            ecog.value.display
          )
        ) 
      )
    }


  implicit val ecogStatusFromFHIR: ObsECOG => dtos.ECOGStatus =
    obs =>
      dtos.ECOGStatus(
        obs.identifier.head,
        obs.subject.identifier,
        obs.effectiveDateTime,
        dtos.Coding(
          dtos.ECOG.withName(obs.valueCodeableConcept.coding.head.code),
          None
//          obs.valueCodeableConcept.coding.head.display
        )
      )
  


  //---------------------------------------------------------------------------
  // Medication mappings
  //---------------------------------------------------------------------------

  import de.bwhc.mtb.data.entry.dtos.{Medication => ATC}


  implicit val medicationSetToFHIR: List[dtos.Coding[ATC]] => MTBMedication = {
    meds =>
      val id = meds.map(_.code.value).reduceLeftOption(_ + "-" + _).getOrElse(java.util.UUID.randomUUID.toString)
      MTBMedication(
        id,
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
  }

/*
  implicit val medicationSetToFHIR: NonEmptyList[dtos.Coding[ATC]] => MTBMedication = {
    meds =>
      val id = meds.map(_.code.value).reduceLeft(_ + "-" + _)
      MTBMedication(
        id,
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
      )
  }

  implicit val medicationListFromFHIR: MTBMedication => NonEmptyList[dtos.Coding[ATC]] = {
    m =>
      m.ingredient
        .map(_.itemCodeableConcept.coding.head)
        .map(c =>
          dtos.Coding(
            dtos.Medication(c.code),
            c.display
          )
        )
  }
*/


  //---------------------------------------------------------------------------
  // Guideline therapy mappings
  //---------------------------------------------------------------------------

  implicit def therapyIdFromIdentifier(id: Identifier) =
    dtos.TherapyId(id.value)

  implicit def therapyIdToIdentifier(id: dtos.TherapyId) =
    Identifier(id.value)


  implicit val prevGLTherapyToFHIR: dtos.PreviousGuidelineTherapy => PreviousGuidelineTherapy = {

    th =>

      val medication = th.medication.getOrElse(List.empty).mapTo[MTBMedication]

      PreviousGuidelineTherapy(
        NonEmptyList.one(th.id),
        th.therapyLine.map(l => Tuple1(TherapyLine(PositiveInt(l.value)))),
        Tuple1(medication),
        MedicationStatement.Status.Unknown,
        LogicalReference[MTBPatient](th.patient),
        NonEmptyList.one(LogicalReference[Condition](th.diagnosis)),
        Reference.contained(medication)
      )
  }

  implicit val prevGLTherapyFromFHIR: PreviousGuidelineTherapy => dtos.PreviousGuidelineTherapy = {
    th =>

      dtos.PreviousGuidelineTherapy(      
        th.identifier.head,
      th.subject.identifier,
        th.reasonReference.head.identifier,
        th.extension.map { case Tuple1(l) => dtos.TherapyLine(l.value) },
        Some(th.contained._1.mapTo[List[dtos.Coding[ATC]]])
      )

  }


  implicit val lastGLTherapyToFHIR:
    dtos.LastGuidelineTherapy => LastGuidelineTherapy = {

    th =>

      import LastGuidelineTherapy._

      val med = th.medication.getOrElse(List.empty).mapTo[MTBMedication]

      LastGuidelineTherapy(
        NonEmptyList.one(th.id),
        th.therapyLine.map(l => Tuple1(TherapyLine(PositiveInt(l.value)))),
        Tuple1(med),
        MedicationStatement.Status.Stopped,
        th.reasonStopped.map(r => List(BasicCodeableConcept(BasicCoding(r.code.toString,None)))),
        LogicalReference[MTBPatient](th.patient),
//        Reference[MTBPatient](th.patient.mapTo[Identifier]),
        NonEmptyList.one(LogicalReference[Condition](th.diagnosis)),
        th.period.map(_.mapTo[OpenEndPeriod[LocalDate]]),
        Reference.contained(med)
      )

  }


  implicit val lastGLTherapyFromFHIR:
   LastGuidelineTherapy => dtos.LastGuidelineTherapy = {
      
    import dtos.GuidelineTherapy.StopReason

    th =>

      dtos.LastGuidelineTherapy(
        th.identifier.head,
        th.subject.identifier,
        th.reasonReference.head.identifier,
        th.extension.map { case Tuple1(ext) => dtos.TherapyLine(ext.value.value) },
        th.period.map(_.mapTo[dtos.OpenEndPeriod[LocalDate]]),
        Some(th.contained._1.mapTo[List[dtos.Coding[ATC]]]),
        th.statusReason.flatMap(_.headOption)
          .map(cc =>
            dtos.Coding[StopReason.Value](
              StopReason.withName(cc.coding.head.code), None
            )
          )
      )

  }


  //---------------------------------------------------------------------------
  // Specimen mappings
  //---------------------------------------------------------------------------

  implicit def specimenIdFromIdentifier(id: Identifier) =
    dtos.Specimen.Id(id.value)

  implicit def specimenIdToIdentifier(id: dtos.Specimen.Id) =
    Identifier(id.value)

  implicit val specimenToFHIR: dtos.Specimen => TumorSpecimen = {
    sp =>

      import TumorSpecimen._

      TumorSpecimen(
        NonEmptyList.one(sp.id),
        NonEmptyList.one(
          SampleDiagnosis(BasicCodeableConcept(sp.icd10.mapTo[BasicCoding[dtos.ICD10GM]]))
        ),
        Reference[MTBPatient](sp.patient),
        sp.collection.map(c => 
          TumorSpecimen.Collection(
            c.date,
            BasicCodeableConcept(BasicCoding(c.localization,None)),
            BasicCodeableConcept(BasicCoding(c.method,None))
          )
        ),
        sp.`type`.map(t => List(BasicCodeableConcept(BasicCoding(t.toString,None))))
      )
  }

  implicit val specimenFromFHIR: TumorSpecimen => dtos.Specimen = {

    import dtos.Specimen.Collection

    sp =>

      dtos.Specimen(
        sp.identifier.head,
        sp.subject.identifier,
        sp.modifierExtension.head.value.coding.head,
        sp.condition.flatMap(_.headOption)
          .map(_.coding.head.code)
          .map(dtos.Specimen.Type.withName _),
        sp.collection.map(c => 
          Collection(
            c.collectedDateTime,
            Collection.Localization.withName(c.bodySite.coding.head.code),
            Collection.Method.withName(c.method.coding.head.code)
          )
        )
      )

  }



  //---------------------------------------------------------------------------
  // Tumor Cell Comntent
  //---------------------------------------------------------------------------

  implicit def tumorCellContentToFHIR(
    implicit subject: LogicalReference[MTBPatient]
  ): dtos.TumorCellContent => ObsTumorCellContent = {
    tc =>

      import ObsTumorCellContent._

      ObsTumorCellContent(
        tc.id.value,
        Observation.Status.Final,
        subject,
        LogicalReference[TumorSpecimen](tc.specimen),
        BasicCodeableConcept(BasicCoding[dtos.TumorCellContent.Method.Value](tc.method.toString,None)),
        SimpleQuantity(tc.value)
      )
  }


  //---------------------------------------------------------------------------
  // Histology mappings
  //---------------------------------------------------------------------------


  implicit def tumorMorphologyIdFromIdentifier(id: Identifier) =
    dtos.TumorMorphology.Id(id.value)

  implicit def tumorMorphologyIdToIdentifier(id: dtos.TumorMorphology.Id) =
    Identifier(id.value)

  implicit val tumorMorphologyToFHIR: dtos.TumorMorphology => ObsTumorMorphology = {
    tm =>
      ObsTumorMorphology(
        rndID.toString,
        Observation.Status.Final,
        LogicalReference[MTBPatient](tm.patient),
        LogicalReference[TumorSpecimen](tm.specimen),
        BasicCodeableConcept(
          BasicCoding(
            tm.value.code.value,
            tm.value.display,
            tm.value.version
          )
        ),
        tm.note.map(Note(_)).map(List(_))
      )
  }

  implicit val histologyReportToFHIR: dtos.HistologyReport => HistologyReport = {
    histoReport =>

      implicit val subject = LogicalReference[MTBPatient](histoReport.patient)

      val tumorContent = histoReport.tumorCellContent.map(_.mapTo[ObsTumorCellContent])
      val morphology   = histoReport.tumorMorphology.map(_.mapTo[ObsTumorMorphology])

      HistologyReport(
        NonEmptyList.one(histoReport.id),
        histoReport.issuedOn,
        DiagnosticReport.Status.Final,
        subject,
//        NonEmptyList.one(Reference[TumorSpecimen](histoReport.specimen)),
        NonEmptyList.one(LogicalReference[TumorSpecimen](histoReport.specimen)),
        tumorContent.map(Reference.contained(_)).toList ++
          morphology.map(Reference.contained(_)),
        (
          morphology,
          tumorContent
        )
      )
  }


  //---------------------------------------------------------------------------
  // Somatic NGS Report mappings
  //---------------------------------------------------------------------------


  implicit def brcanessToFHIR(
    implicit
    subject: LogicalReference[MTBPatient],
    specimen: LogicalReference[TumorSpecimen]
  ): dtos.SomaticNGSReport.BRCAness => ObsBRCAness = {
    brca =>

      import ObsBRCAness._

      ObsBRCAness(
        rndID.toString,
        Observation.Status.Final,
        subject,
        specimen,
        SimpleQuantity(brca.value)
      )
  }


  implicit def tmbToFHIR(
    implicit
    subject: LogicalReference[MTBPatient],
    specimen: LogicalReference[TumorSpecimen]
  ): dtos.SomaticNGSReport.TMB => ObsTMB = {
    tmb =>

      import ObsTMB._

      ObsTMB(
        rndID.toString,
        Observation.Status.Final,
        subject,
        specimen,
        SimpleQuantity(tmb.value,Some("mut/Mb"))
      )
  }


  implicit def msiToFHIR(
    implicit
    subject: LogicalReference[MTBPatient],
    specimen: LogicalReference[TumorSpecimen]
  ): dtos.SomaticNGSReport.MSI => ObsMSI = {
    msi =>

      import ObsMSI._

      ObsMSI(
        rndID.toString,
        Observation.Status.Final,
        subject,
        specimen,
        SimpleQuantity(msi.value)
      )
  }





  implicit def cosmicIdFromIdentifier(id: Identifier) =
    dtos.Variant.CosmicId(id.value)

  implicit def cosmicIdToIdentifier(id: dtos.Variant.CosmicId) =
    Identifier(id.value)


  implicit def simpleVariantToFHIR(
    implicit subject: LogicalReference[MTBPatient]
  ): dtos.SimpleVariant => SimpleVariant = {

    snv =>

    import ObsVariant._
    import SimpleVariant._
      
      SimpleVariant(
        snv.id.value,
        snv.cosmicId.map(_.mapTo[Identifier]).map(List(_)),
        Observation.Status.Final,
        subject,
        SimpleVariant.Components(
          Chromosome(snv.chromosome.value),
          NonEmptyList.one(
            GeneStudied(
              BasicCodeableConcept(BasicCoding[HGNC](snv.gene.code.value,snv.gene.display))
            )
          ),
          ExactStartEnd(
            LBoundedRange(snv.startEnd.start.toDouble,snv.startEnd.end.map(_.toDouble))
          ),
          RefAllele(snv.refAllele.value),
          AltAllele(snv.altAllele.value),
          DNAChange(
            BasicCodeableConcept(BasicCoding[HGVS](snv.dnaChange.code.value,None))
          ),
          AminoAcidChange(
            BasicCodeableConcept(BasicCoding[HGVS](snv.aminoAcidChange.code.value,None))
          ),
          snv.dbSNPId.map(v => DbSNPId(BasicCodeableConcept(BasicCoding[dbSNP](v.value,None)))),
          SampleAllelicFrequency(
            SimpleQuantity(snv.allelicFrequency.value)
          ),
          AllelicReadDepth(
            SimpleQuantity(snv.readDepth.value)
          )
        ),
        NonEmptyList.one(
          BasicCodeableConcept(
            BasicCoding[ClinVar](snv.interpretation.code.value,None)
          ),
        )
      )
  }  


  implicit val ngsReportToFHIR: dtos.SomaticNGSReport => SomaticNGSReport = {

    ngsReport =>

      import ExtMetaData._

//      implicit val subject  = Reference[MTBPatient](ngsReport.patient)
//      implicit val specimen = Reference[TumorSpecimen](ngsReport.specimen)
      implicit val subject  = LogicalReference[MTBPatient](ngsReport.patient)
      implicit val specimen = LogicalReference[TumorSpecimen](ngsReport.specimen)

      val tcc      = ngsReport.tumorCellContent.mapTo[ObsTumorCellContent]
      val tmb      = ngsReport.tmb.mapTo[ObsTMB]
      val msi      = ngsReport.msi.map(_.mapTo[ObsMSI])
      val brcaness = ngsReport.brcaness.map(_.mapTo[ObsBRCAness])
      val snvs     = ngsReport.simpleVariants.getOrElse(List.empty).map(_.mapTo[SimpleVariant])

      SomaticNGSReport(
        NonEmptyList.one(Identifier(ngsReport.id.value)),
        ngsReport.issueDate,
        DiagnosticReport.Status.Final,
        ngsReport.metadata.map {
         case dtos.SomaticNGSReport.MetaData(kitType,manufacturer,seq,ref,pipeline) =>
            ExtMetaData(
              KitType(kitType),
              KitManufacturer(manufacturer),
              Sequencer(seq),
              RefGenome(ref.value),
              pipeline.map(Pipeline(_))
            )
        },
/*
        (
          ExtSequencingType(BasicCoding[dtos.SomaticNGSReport.SequencingType](ngsReport.sequencingType.value)),
          ngsReport.metadata.map {
           case dtos.SomaticNGSReport.MetaData(kitType,manufacturer,seq,ref,pipeline) =>
              ExtMetaData(
                KitType(kitType),
                KitManufacturer(manufacturer),
                Sequencer(seq),
                RefGenome(ref.value),
                pipeline.map(Pipeline(_))
              )
          }
        ),
*/
        subject,
        NonEmptyList.one(specimen),
        NonEmptyList.of(
          Reference.contained(tcc),
          Reference.contained(tmb),
        ) ++
          msi.map(Reference.contained(_)).toList ++
          brcaness.map(Reference.contained(_)).toList ++
          snvs.map(Reference.contained(_)),
        SomaticNGSReport.Results(
          tcc,
          tmb,
          msi,
          brcaness,
          snvs          
        )
      )

  }


  //---------------------------------------------------------------------------
  // CarePlan / TherapyRecommendation mappings
  //---------------------------------------------------------------------------
  
  implicit def therapyRecIdFromIdentifier(id: Identifier) =
    dtos.TherapyRecommendation.Id(id.value)

  implicit def therapyRecIdToIdentifier(id: dtos.TherapyRecommendation.Id) =
    Identifier(id.value)


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

        val med = rec.medication.getOrElse(List.empty).mapTo[MTBMedication]

        TherapyRecommendation(
          NonEmptyList.one(rec.id),
          rec.levelOfEvidence.map( loe =>
            Tuple1(
              LoE(
                LoE.Grade(BasicCoding(loe.grading.code,None)),
                loe.addendums.getOrElse(Set.empty)
                  .map(add => LoE.Addendum(BasicCoding(add.code,None)))
              )
            )
          ),
          Tuple1(med),
          rec.priority.map(_.mapTo[MedicationRequest.Priority.Value]),
          MedicationRequest.Status.Unknown,          
          MedicationRequest.Intent.Proposal,
          rec.issuedOn,
          LogicalReference[MTBPatient](rec.patient),
          NonEmptyList.one(LogicalReference[Condition](rec.diagnosis)),
          Reference.contained(med),
     None //TODO: map supportingVariant 
        )

  }


  implicit val therapyRecommendationFromFHIR:
    TherapyRecommendation => dtos.TherapyRecommendation = {

      rec =>
 
        dtos.TherapyRecommendation(       
          rec.identifier.head,        
          rec.subject.identifier,
          rec.reasonReference.head.identifier,
          rec.authoredOn,
          Some(rec.contained._1.mapTo[List[dtos.Coding[ATC]]]),
          rec.priority.map(_.mapTo[dtos.TherapyRecommendation.Priority.Value]),
          rec.extension.map { case Tuple1(loe) =>
            dtos.LevelOfEvidence(
              dtos.Coding(dtos.LevelOfEvidence.Grading.withName(loe.grade.value.code),None),
              Option(loe.addendums)
                .filterNot(_.isEmpty)
                .map(_.map(_.value.code))
                .map(_.map(dtos.LevelOfEvidence.Addendum.withName _))
                .map(_.map(dtos.Coding(_,None)))
            )
          },
  None, //TODO: NGS-Report ref.
  None, //TODO: Variant ref.
        )
    }





  implicit def counsellingReqIdFromIdentifier(id: Identifier) =
    dtos.GeneticCounsellingRequest.Id(id.value)

  implicit def counsellingReqIdToIdentifier(id: dtos.GeneticCounsellingRequest.Id) =
    Identifier(id.value)


  implicit val counsellingReqToFHIR: dtos.GeneticCounsellingRequest => CounsellingRequest = {
    req =>

     CounsellingRequest(
       NonEmptyList.one(req.id),
       ServiceRequest.Status.Unknown,
       ServiceRequest.Intent.Proposal,
       LogicalReference[MTBPatient](req.patient),
       req.issuedOn,
       NonEmptyList.one(Note(req.reason))
     )
  }


  implicit def rebiopsyReqIdFromIdentifier(id: Identifier) =
    dtos.RebiopsyRequest.Id(id.value)

  implicit def rebiopsyReqIdToIdentifier(id: dtos.RebiopsyRequest.Id) =
    Identifier(id.value)


  implicit val rebiopsyReqToFHIR: dtos.RebiopsyRequest => RebiopsyRequest = {
    req =>

     RebiopsyRequest(
       NonEmptyList.one(req.id),
       ServiceRequest.Status.Unknown,
       ServiceRequest.Intent.Proposal,
       LogicalReference[MTBPatient](req.patient),
       req.issuedOn,
       NonEmptyList.one(LogicalReference[TumorSpecimen](req.specimen))
     )
  }




  implicit def carePlanIdFromIdentifier(id: Identifier) =
    dtos.CarePlan.Id(id.value)

  implicit def carePlanIdToIdentifier(id: dtos.CarePlan.Id) =
    Identifier(id.value)



  implicit val carePlanToFHIR: dtos.CarePlan => MTBCarePlan = {

    cp => 
      MTBCarePlan(        
        NonEmptyList.one(cp.id.mapTo[Identifier]),
        CarePlan.Status.Unknown,
        CarePlan.Intent.Proposal,
        cp.issuedOn,
        LogicalReference[MTBPatient](cp.patient),
        NonEmptyList.one(LogicalReference[Diagnosis](cp.diagnosis)),
        cp.description,
        (
          cp.recommendations.getOrElse(List.empty)
            .map(LogicalReference[TherapyRecommendation](_)) ++
          cp.geneticCounsellingRequest
            .map(LogicalReference[CounsellingRequest](_)) ++
          cp.rebiopsyRequests.getOrElse(List.empty)
            .map(LogicalReference[CounsellingRequest](_))
        )
        .map(MTBCarePlan.Activity(_))
      )

  }

  implicit val carePlanFromFHIR: MTBCarePlan => dtos.CarePlan = {

    cp => 
      dtos.CarePlan(
        dtos.CarePlan.Id(cp.identifier.head.value),
        cp.subject.identifier,
        cp.addresses.head.identifier,
        cp.created,
        cp.description,
   None, //TODO TODO
        Some(cp.activity.map(_.reference.identifier.mapTo[dtos.TherapyRecommendation.Id])).filterNot(_.isEmpty),
        None, //TODO TODO
   None, //TODO TODO
   None //TODO TODO
      )

  }


  //---------------------------------------------------------------------------
  // Claim / ClaimResponse mappings
  //---------------------------------------------------------------------------

  implicit val claimToFHIR: dtos.Claim => ClaimDTO = {

    claim =>

      ClaimDTO(
        NonEmptyList.one(Identifier(claim.id.value)),
        claim.issuedOn,
        BasicCodeableConcept(BasicCoding(Claim.Type.Institutional)),
        Claim.Use.Claim,
        BasicCodeableConcept(BasicCoding(ProcessPriority.Normal)),
        Claim.Status.Draft,
        LogicalReference[TherapyRecommendation](claim.therapy),
        LogicalReference[Patient](claim.patient),
        LogicalReference[Organization](Identifier("TODO")),  //TODO
      )
  }

  implicit val claimFromFHIR: ClaimDTO => dtos.Claim = {
    claim =>
      dtos.Claim(
        dtos.Claim.Id(claim.identifier.head.value),
        dtos.Patient.Id(claim.patient.identifier.value),
        claim.created,
        dtos.TherapyRecommendation.Id(claim.prescription.identifier.value),
      )
  }



  implicit def claimResponseToFHIR(
    implicit patient: dtos.Patient
  ): dtos.ClaimResponse => ClaimResponseDTO = {

    cr =>

      ClaimResponseDTO(
        NonEmptyList.one(Identifier(cr.id.value)),
        cr.issuedOn,
        BasicCodeableConcept(BasicCoding(Claim.Type.Institutional)),
        Claim.Use.Claim,
        Claim.Status.Draft,
        LogicalReference[Patient](cr.patient),
        LogicalReference[Claim](Identifier(cr.claim.value)),
        LogicalReference[Organization](Identifier(patient.insurance.map(_.value).getOrElse("Unknown"))),  //TODO
        ClaimResponse.Outcome.Partial  //TODO
      )
  }

  implicit val claimResponseFromFHIR: ClaimResponseDTO => dtos.ClaimResponse = {
    cr =>
      dtos.ClaimResponse(
        dtos.ClaimResponse.Id(cr.identifier.head.value),
        dtos.Claim.Id(cr.request.identifier.value),
        dtos.Patient.Id(cr.patient.identifier.value),
        cr.created,
        dtos.ClaimResponse.Status.Accepted,  //TODO
        None  //TODO
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
      val subject    = LogicalReference[MTBPatient](molTh.patient)
      val basedOn    = NonEmptyList.one(LogicalReference[TherapyRecommendation](molTh.basedOn))
      val note       = molTh.note.map(Note(_)).map(List(_))

      molTh match {

        case th: dtos.NotDoneTherapy => {
          NotTakenMolecularTherapy(
            identifier,
            basedOn,
            molTh.recordedOn,
            subject,
            Reference[MTBMedication]("DUMMY"),
            NonEmptyList.one(
              BasicCodeableConcept(BasicCoding(th.notDoneReason.code.toString,None))
            ),
            note
          )
        }

        case th: dtos.StoppedTherapy => {

          val medication = th.medication.getOrElse(List.empty).mapTo[MTBMedication]

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
              BasicCodeableConcept(BasicCoding(th.reasonStopped.code.toString,None))
            ),
            note
          )
        }

        case th: dtos.CompletedTherapy => {

          val medication = th.medication.getOrElse(List.empty).mapTo[MTBMedication]

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

          val medication = th.medication.getOrElse(List.empty).mapTo[MTBMedication]

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

      val id      = dtos.TherapyId(molTh.identifier.head.value)
      val note    = molTh.note.flatMap(_.headOption).map(_.text)

      molTh match {

        case th: NotTakenMolecularTherapy => {
          dtos.NotDoneTherapy(
            id,
            th.subject.identifier, 
            th.dateAsserted,
            th.basedOn.head.identifier,
            dtos.Coding(NotDoneReason.withName(th.statusReason.head.coding.head.code),None),
            note
          )
        }

        case th: StoppedMolecularTherapy => {
          dtos.StoppedTherapy(
            id,
            th.subject.identifier, 
            molTh.dateAsserted,
            th.basedOn.head.identifier,
            dtos.ClosedPeriod(th.effectivePeriod.start,th.effectivePeriod.end),
            Some(th.contained._1.mapTo[List[dtos.Coding[ATC]]]),
            th.dosage.flatMap(_.headOption).map(_.mapTo[dtos.Dosage.Value]),
            dtos.Coding(StopReason.withName(th.statusReason.head.coding.head.code),None),
            note
          )
        }

        case th: CompletedMolecularTherapy => {
          dtos.CompletedTherapy(
            id,
            th.subject.identifier, 
            molTh.dateAsserted,
            th.basedOn.head.identifier,
            dtos.ClosedPeriod(th.effectivePeriod.start,th.effectivePeriod.end),
            Some(th.contained._1.mapTo[List[dtos.Coding[ATC]]]),
            th.dosage.flatMap(_.headOption).map(_.mapTo[dtos.Dosage.Value]),
            note
          )
        }

        case th: ActiveMolecularTherapy => {
          dtos.OngoingTherapy(
            id,
            th.subject.identifier, 
            molTh.dateAsserted,
            th.basedOn.head.identifier,
            dtos.OpenEndPeriod(th.effectivePeriod.start),
            Some(th.contained._1.mapTo[List[dtos.Coding[ATC]]]),
            th.dosage.flatMap(_.headOption).map(_.mapTo[dtos.Dosage.Value]),
            note
          )
        }

      }
  }


  implicit val molTherapyDocFromFHIR:
    MolecularTherapyHistory => dtos.MolecularTherapyDocumentation = {
      bundle =>
        dtos.MolecularTherapyDocumentation(
          bundle.entry.list.map(_.mapTo[dtos.MolecularTherapy])
        )
    }


  //---------------------------------------------------------------------------
  // Response mappings
  //---------------------------------------------------------------------------

  implicit def responseIdFromIdentifier(id: Identifier) =
    dtos.Response.Id(id.value)

  implicit def responseIdToIdentifier(id: dtos.Response.Id) =
    Identifier(id.value)


  implicit val responseToFHIR: dtos.Response => ObsRECIST = {
    resp =>
      ObsRECIST(
        NonEmptyList.one(resp.id),
        Observation.Status.Final,
        NonEmptyList.one(LogicalReference[MedicationStatement](resp.therapy)),
        resp.effectiveDate,
        LogicalReference[MTBPatient](resp.patient),
        BasicCodeableConcept(BasicCoding(resp.value.code,None)) 
      )
  }

  implicit val responseFromFHIR: ObsRECIST => dtos.Response = {
    resp =>
      dtos.Response(
        resp.identifier.head,
        resp.subject.identifier,
        resp.partOf.head.identifier,
        resp.effectiveDate,
        dtos.Coding(
          dtos.RECIST.withName(resp.valueCodeableConcept.coding.head.code),
          None
        )
      )
  }


  //---------------------------------------------------------------------------
  // MTB File mappings
  //---------------------------------------------------------------------------

  implicit val mtbFileToFHIR: dtos.MTBFile => MTBFileBundle = {

    mtbfile =>

      implicit val pat = mtbfile.patient
      
      MTBFileBundle(
        MTBFileEntries(
          EntryOf(mtbfile.patient.mapTo[MTBPatient]),
          EntryOf(mtbfile.episode.mapTo[MTBEpisode]),
          EntryOf(mtbfile.consent.mapTo[BwHCConsent]),
          mtbfile.diagnoses.getOrElse(List.empty).map(_.mapTo[Diagnosis]).map(EntryOf(_)),
          mtbfile.familyMemberDiagnoses.getOrElse(List.empty).map(_.mapTo[FamilyMemberHistoryDTO]).map(EntryOf(_)),
          mtbfile.previousGuidelineTherapies.getOrElse(List.empty).map(_.mapTo[PreviousGuidelineTherapy]).map(EntryOf(_)),
          mtbfile.lastGuidelineTherapy.map(_.mapTo[LastGuidelineTherapy]).map(EntryOf(_)),
          mtbfile.ecogStatus.getOrElse(List.empty).map(_.mapTo[ObsECOG]).map(EntryOf(_)),
          mtbfile.specimens.getOrElse(List.empty).map(_.mapTo[TumorSpecimen]).map(EntryOf(_)),
          mtbfile.histologyReports.getOrElse(List.empty).map(_.mapTo[HistologyReport]).map(EntryOf(_)),
          mtbfile.ngsReports.getOrElse(List.empty).map(_.mapTo[SomaticNGSReport]).map(EntryOf(_)),
          mtbfile.carePlans.getOrElse(List.empty).map(_.mapTo[MTBCarePlan]).map(EntryOf(_)),
          mtbfile.recommendations.getOrElse(List.empty).map(_.mapTo[TherapyRecommendation]).map(EntryOf(_)),
          mtbfile.geneticCounsellingRequests.getOrElse(List.empty).map(_.mapTo[CounsellingRequest]).map(EntryOf(_)),
          mtbfile.rebiopsyRequests.getOrElse(List.empty).map(_.mapTo[RebiopsyRequest]).map(EntryOf(_)),
          mtbfile.claims.getOrElse(List.empty).map(_.mapTo[ClaimDTO]).map(EntryOf(_)),
          mtbfile.claimResponses.getOrElse(List.empty).map(_.mapTo[ClaimResponseDTO]).map(EntryOf(_)),
          mtbfile.molecularTherapies.getOrElse(List.empty).map(_.mapTo[MolecularTherapyHistory]).map(EntryOf(_)),
          mtbfile.responses.getOrElse(List.empty).map(_.mapTo[ObsRECIST]).map(EntryOf(_))
        )
      )

    }


  implicit val mtbFileFromFHIR: MTBFileBundle => dtos.MTBFile = {
    bundle =>

      val patient = bundle.entry.patient.mapTo[dtos.Patient]

      dtos.MTBFile(
        patient,
        bundle.entry.consent.mapTo[dtos.Consent],
        bundle.entry.episode.mapTo[dtos.MTBEpisode],
        Some(bundle.entry.diagnoses.map(_.mapTo[dtos.Diagnosis])).filterNot(_.isEmpty),
        Some(bundle.entry.familyMemberDiagnoses.map(_.mapTo[dtos.FamilyMemberDiagnosis])).filterNot(_.isEmpty),
        Some(bundle.entry.previousGLTherapies.map(_.mapTo[dtos.PreviousGuidelineTherapy])),
        bundle.entry.lastGLTherapy.map(_.mapTo[dtos.LastGuidelineTherapy]),
        Some(bundle.entry.ecogs.map(_.mapTo[dtos.ECOGStatus])),
        Some(bundle.entry.specimens.map(_.mapTo[dtos.Specimen])),
    None, //TODO
    None, //TODO
    None, //TODO
        Some(bundle.entry.carePlans.map(_.mapTo[dtos.CarePlan])),
        Some(bundle.entry.therapyRecommendations.map(_.mapTo[dtos.TherapyRecommendation])),
    None, //TODO
    None, //TODO
    None, //TODO
    None, //TODO
    None, //TODO
    None, //TODO
        Some(bundle.entry.molecularTherapies.map(_.mapTo[dtos.MolecularTherapyDocumentation])),
        Some(bundle.entry.responses.map(_.mapTo[dtos.Response])),
      )

  }

}
