package de.bwhc.fhir



import java.time.temporal.Temporal
import java.time.LocalDate
import java.util.UUID.{randomUUID => rndID}

import cats.data.NonEmptyList

import de.bwhc.mtb.data.entry.dtos

import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Bundle.EntryOf

import CodingSystems._


object Mappings
{

  import scala.language.implicitConversions


  implicit class MappingSyntax[T](val t: T) extends AnyVal
  {
    def mapTo[U](implicit f: T => U) = f(t)
  }


  implicit class FuntorMappingSyntax[T,F[_]: cats.Functor](val ft: F[T])
  {
    import cats.syntax.functor._

    def mapToF[U](implicit f: T => U) = ft.map(f)
  }


  implicit class MapOps[K,V](val map: Map[K,V]) extends AnyVal
  {
    def invert: Map[V,K] =
      map.toSeq
       .foldLeft(List.empty[(V,K)])((l,kv) => (kv._2,kv._1) :: l)
       .toMap
  }


  import cats.instances.list._
  import cats.instances.option._


  implicit def mapBundleEntry[R <: Resource,T](
    implicit f: R => T
  ): Bundle.EntryElement with Bundle.Entry.resource[R] => T = {
    entry => entry.resource.mapTo[T]
  }



  implicit def openEndPeriodToFHIR[T <: Temporal]: dtos.OpenEndPeriod[T] => OpenEndPeriod[T] =
    p => OpenEndPeriod(p.start,p.end)


  implicit def openEndPeriodFromFHIR[T <: Temporal]: OpenEndPeriod[T] => dtos.OpenEndPeriod[T] =
    p => dtos.OpenEndPeriod(p.start,p.end)



  implicit def codingToFHIR[T: org.hl7.fhir.r4.CodingSystem]: dtos.Coding[T] => CodingStatic[T] = 
    coding =>
      CodingStatic[T](
        coding.code.toString,
        coding.display,
        coding.version
      )


  implicit val icd10CodingToFHIR: dtos.Coding[dtos.ICD10GM] => CodingStatic[dtos.ICD10GM] = 
    coding =>
      CodingStatic[dtos.ICD10GM](
        coding.code.value,
        coding.display,
        coding.version
      )

  implicit val icd10CodingFromFHIR: CodingStatic[dtos.ICD10GM] => dtos.Coding[dtos.ICD10GM] = 
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
        CodeableConceptStatic(CodingStatic(Consent.Scope.Research)) 
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
        diag.icd10.map(icd => CodeableConceptStatic(icd.mapTo[CodingStatic[dtos.ICD10GM]])),
        diag.icdO3T.map(c =>
          List(
            CodeableConceptStatic(              
              CodingStatic[dtos.ICDO3T](
                c.code.value,
                c.display,
                c.version
              )
            )

          )
        ),
        Diagnosis.Stages(
          diag.statusHistory.map(sts =>
            sts.map(st =>
              Diagnosis.Stage(
                Some(List(Diagnosis.Stage.Date(st.date))),
                CodeableConceptStatic( 
                  CodingStatic[dtos.Diagnosis.Status.Value](st.status.toString,None,None)
                )
              ) 
            )
          )
          .getOrElse(List.empty[Diagnosis.Stage[dtos.Diagnosis.Status.Value]]), 
          diag.whoGrade.map(who =>
            Diagnosis.Stage(
              diag.recordedOn.map(d => List(Diagnosis.Stage.Date(d))),
              CodeableConceptStatic(
                CodingStatic[dtos.WHOGrade.Value](who.code.toString,who.display,None)
              )
            )
          )
        ),
        diag.histologyResults.map(
          ids => ids.map(
            id => Diagnosis.HistologyEvidence(NonEmptyList.one(Reference[ObsHistology](id)))
          )
        ),
        diag.guidelineTreatmentStatus.map(
          gl =>
              DiagnosisProfile.GuidelineTreatmentStatus(
                CodingStatic[dtos.GuidelineTreatmentStatus.Value](gl.toString,None,None)
              )
        )
        .map(List(_))
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
        diag.stage.whoGrade
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
          diag.stage.tumorStages
            .map(st =>
              dtos.Diagnosis.StatusOnDate( 
                dtos.Diagnosis.Status.withName(st.summary.coding.head.code),
                st.extension.get.head.value
              )
            )
        ),
        diag.extension.flatMap(_.headOption)
          .map(
            ext => dtos.GuidelineTreatmentStatus.withName(ext.value.code) 
          )
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
          CodeableConceptStatic(CodingStatic(diag.relationship.code))
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
        CodeableConceptStatic(
          CodingStatic[dtos.ECOG.Value](
            ecog.value.code.toString,
            dtos.ValueSet[dtos.ECOG.Value].displayOf(ecog.value.code),
            None
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
        )
      )
  


  //---------------------------------------------------------------------------
  // Medication mappings
  //---------------------------------------------------------------------------

  import de.bwhc.mtb.data.entry.dtos.{Medication => ATC}

  implicit val medicationSystemToFHIR =
    Map[dtos.Medication.System.Value,String](
      dtos.Medication.System.ATC          -> "http://fhir.de/CodeSystem/dimdi/atc",
      dtos.Medication.System.Unregistered -> "Unregistered" 
    )
  
  implicit val medicationSystemFromFHIR = medicationSystemToFHIR.invert


  implicit val medicationSetToFHIR: List[dtos.Medication.Coding] => MTBMedication = {

    meds =>

      val id =
        meds.map(_.code.value)
          .reduceLeftOption(_ + "-" + _)
          .getOrElse(java.util.UUID.randomUUID.toString)

      MTBMedication(
        id,
        meds.map(
          m =>
            MTBMedication.Ingredient(
              CodeableConceptDynamic(
                CodingDynamic(
                  m.code.value,
                  m.display,
                  m.system.mapTo[String],
                  m.version
                )
              )
          )
        )
      )
  }

  implicit val medicationListFromFHIR: MTBMedication => List[dtos.Medication.Coding] = {
    m =>
      m.ingredient
        .map(_.itemCodeableConcept.coding.head)
        .map(c =>
          dtos.Medication.Coding(
            dtos.Medication.Code(c.code),
            c.system.mapTo[dtos.Medication.System.Value],
            c.display,
            c.version
          )
        )
  }


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
        th.therapyLine.map(l => List(TherapyLine(PositiveInt(l.value)))),
        ContainedMedication(medication),
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
        th.extension.map { l => dtos.TherapyLine(l.head.value) },
        Some(th.contained.medication.mapTo[List[dtos.Medication.Coding]])
      )

  }


  implicit val lastGLTherapyToFHIR: dtos.LastGuidelineTherapy => LastGuidelineTherapy = {

    th =>

      import LastGuidelineTherapy._

      val medication = th.medication.getOrElse(List.empty).mapTo[MTBMedication]

      LastGuidelineTherapy(
        NonEmptyList.one(th.id),
        th.therapyLine.map(l => List(TherapyLine(PositiveInt(l.value)))),
        ContainedMedication(medication),
        MedicationStatement.Status.Stopped,
        th.reasonStopped.map(r => List(CodeableConceptStatic(CodingStatic(r.code.toString,None,None)))),
        LogicalReference[MTBPatient](th.patient),
        NonEmptyList.one(LogicalReference[Condition](th.diagnosis)),
        th.period.mapToF[OpenEndPeriod[LocalDate]],
        Reference.contained(medication)
      )

  }


  implicit val lastGLTherapyFromFHIR: LastGuidelineTherapy => dtos.LastGuidelineTherapy = {
      
    import dtos.GuidelineTherapy.StopReason

    th =>

      dtos.LastGuidelineTherapy(
        th.identifier.head,
        th.subject.identifier,
        th.reasonReference.head.identifier,
        th.extension.flatMap(_.headOption).map(l => dtos.TherapyLine(l.value.value)),
        th.effectivePeriod.mapToF[dtos.OpenEndPeriod[LocalDate]],
        Some(th.contained.medication.mapTo[List[dtos.Medication.Coding]]),
        th.statusReason.flatMap(_.headOption)
          .map(cc =>
            dtos.Coding[StopReason.Value](
              StopReason.withName(cc.coding.head.code),
              None
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
          SampleDiagnosis(CodeableConceptStatic(sp.icd10.mapTo[CodingStatic[dtos.ICD10GM]]))
        ),
        Reference[MTBPatient](sp.patient),
        sp.collection.map(c => 
          TumorSpecimen.Collection(
            c.date,
            CodeableConceptStatic(CodingStatic(c.localization,None)),
            CodeableConceptStatic(CodingStatic(c.method,None))
          )
        ),
        sp.`type`.map(t => List(CodeableConceptStatic(CodingStatic(t.toString,None,None))))
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
        CodeableConceptStatic(CodingStatic[dtos.TumorCellContent.Method.Value](tc.method.toString,None,None)),
        SimpleQuantity(tc.value)
      )
  }

  implicit def tumorCellContentFromFHIR: ObsTumorCellContent => dtos.TumorCellContent = {
    obs =>
      dtos.TumorCellContent(
        dtos.TumorCellContent.Id(obs.id),
        obs.specimen.identifier,
        dtos.TumorCellContent.Method.withName(obs.method.coding.head.code),
        obs.valueQuantity.value
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
        CodeableConceptStatic(
          CodingStatic(
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
        NonEmptyList.one(LogicalReference[TumorSpecimen](histoReport.specimen)),
        (
         tumorContent.map(Reference.contained(_)) ++
         morphology.map(Reference.contained(_))
        ).toList,
        HistologyReport.Results(
          morphology,
          tumorContent
        )
      )
  }


  implicit val tumorMorphologyFromFHIR: ObsTumorMorphology => dtos.TumorMorphology = {
    obs =>
      dtos.TumorMorphology(
        dtos.TumorMorphology.Id(obs.id),
        obs.subject.identifier,
        obs.specimen.identifier,
        dtos.Coding(
          dtos.ICDO3M(obs.valueCodeableConcept.coding.head.code),
          obs.valueCodeableConcept.coding.head.display,
          obs.valueCodeableConcept.coding.head.version
        ),
        obs.note.flatMap(_.headOption).map(_.text)
      )
  }


  implicit val histologyReportFromFHIR: HistologyReport => dtos.HistologyReport = {
    report =>
      dtos.HistologyReport(
        dtos.HistologyReport.Id(report.identifier.head.value),
        report.subject.identifier,
        dtos.Specimen.Id(report.specimen.head.identifier.value),
        report.issued,
        report.contained.tumorMorphology.mapToF[dtos.TumorMorphology],
        report.contained.tumorCellContent.mapToF[dtos.TumorCellContent],
     )
  }

  //---------------------------------------------------------------------------
  // Molecular Pathology Report mappings
  //---------------------------------------------------------------------------

  implicit val molPathoReportToFHIR: dtos.MolecularPathologyFinding => MolecularPathologyReport = {
    report =>

      MolecularPathologyReport(
        NonEmptyList.one(Identifier(report.id.value)),
        report.issuedOn,
        DiagnosticReport.Status.Final,
        LogicalReference[MTBPatient](report.patient),
        NonEmptyList.one(LogicalReference[TumorSpecimen](report.specimen)),
        report.performingInstitute.map(
          dept => LogicalReference[Organization](Identifier(dept.value))
        )
        .map(List(_)),
        report.note
      )

  }

  implicit val molPathoReportFromFHIR: MolecularPathologyReport => dtos.MolecularPathologyFinding = {
    report =>

      dtos.MolecularPathologyFinding(
        dtos.MolecularPathologyFinding.Id(report.identifier.head.value),
        report.subject.identifier,
        report.specimen.head.identifier,
        report.performer.flatMap(_.headOption).map(
          ref => dtos.PathologyDept(ref.identifier.value)
        ),
        report.issued,
        report.conclusion
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


  implicit val geneCodingToCodeableConcept: dtos.Gene.Coding => CodeableConceptDynamic = {

    gene =>

      CodeableConceptDynamic(
        NonEmptyList.fromListUnsafe(
          gene.ensemblId.map(
            id =>
              CodingDynamic(
                id.value,
                gene.symbol.map(_.value),
                CodingSystem[ObsVariant.Ensembl].uri.toString,
                None
              )
          )
          .toList ++
          gene.hgncId.map(
            id =>
              CodingDynamic(
                id.value,
                gene.symbol.map(_.value),
                CodingSystem[ObsVariant.HGNC].uri.toString,
                None
              )
          ),
        ),
        None
      )
  }


  implicit val geneCodingFromCodeableConcept: CodeableConceptDynamic => dtos.Gene.Coding = {
    cc =>
      dtos.Gene.Coding(
        cc.coding
          .find(_.system == CodingSystem[ObsVariant.Ensembl].uri.toString)
          .map(c => dtos.Gene.EnsemblId(c.code)),
        cc.coding
          .find(_.system == CodingSystem[ObsVariant.HGNC].uri.toString)
          .map(c => dtos.Gene.HgncId(c.code)),
        None,
        None
      )
  }


  implicit def simpleVariantToFHIR(
    implicit subject: LogicalReference[MTBPatient]
  ): dtos.SimpleVariant => SimpleVariant = {

    snv =>

    import ObsVariant._
    import SimpleVariant._
      
      SimpleVariant(
        snv.id.value,
        snv.cosmicId.mapToF[Identifier].map(List(_)),
        Observation.Status.Final,
        subject,
        SimpleVariant.Components(
          Chromosome(snv.chromosome.value),
          snv.gene.mapToF[CodeableConceptDynamic]
            .map(GeneStudied(_))
            .toList,
          ExactStartEnd(
            LBoundedRange(snv.startEnd.start.toDouble,snv.startEnd.end.map(_.toDouble))
          ),
          RefAllele(snv.refAllele.value),
          AltAllele(snv.altAllele.value),
          snv.dnaChange.map(
            dnaChg =>
              DNAChange(
                CodeableConceptStatic(CodingStatic[HGVS](dnaChg.code.value,None,None))
             )
          ),
          snv.aminoAcidChange.map(
            aaChg =>
              AminoAcidChange(
                CodeableConceptStatic(CodingStatic[HGVS](aaChg.code.value,None,None))
              )
          ),
          snv.dbSNPId.map(v => DbSNPId(CodeableConceptStatic(CodingStatic[dbSNP](v.value,None,None)))),
          SampleAllelicFrequency(
            SimpleQuantity(snv.allelicFrequency.value)
          ),
          AllelicReadDepth(
            SimpleQuantity(snv.readDepth.value)
          )
        ),
        NonEmptyList.one(
          CodeableConceptStatic(
            CodingStatic[ClinVar](snv.interpretation.code.value,None,None)
          ),
        )
      )
  } 
 
  implicit def simpleVariantFromFHIR: SimpleVariant => dtos.SimpleVariant = {
    snv =>

      import dtos.Variant._
      import dtos.SimpleVariant._

      dtos.SimpleVariant(
        Id(snv.id),
        dtos.Chromosome(snv.component.chromosome.valueString),
        snv.component.geneStudied.headOption.map(_.valueCodeableConcept).mapToF[dtos.Gene.Coding],
        StartEnd(
          snv.component.exactStartEnd.valueRange.low.value.toLong,
          snv.component.exactStartEnd.valueRange.high.map(_.value.toLong),
        ),
        Allele(snv.component.refAllele.valueString),
        Allele(snv.component.altAllele.valueString),
        snv.component.dnaChange.map(
          c =>
            dtos.Coding(
              DNAChange(c.valueCodeableConcept.coding.head.code),
              c.valueCodeableConcept.coding.head.display
            )
        ), 
        snv.component.aminoAcidChange.map(
          c =>
            dtos.Coding(
              AminoAcidChange(c.valueCodeableConcept.coding.head.code),
              c.valueCodeableConcept.coding.head.display
            )
          
        ), 
        AllelicReadDepth(snv.component.allelicReadDepth.valueQuantity.value.toInt),
        AllelicFrequency(snv.component.sampleAllelicFrequency.valueQuantity.value),
        snv.identifier.flatMap(_.headOption).mapToF[CosmicId],
        snv.component.dbSNPId.map(
          c => DbSNPId(c.valueCodeableConcept.coding.head.code)
        ),
        dtos.Coding(
          Interpretation(snv.interpretation.head.coding.head.code),
          None
        )        
      )
  }


  implicit def cnvToFHIR(
    implicit subject: LogicalReference[MTBPatient]
  ): dtos.CNV => CopyNumberVariant = {

    cnv =>

    import ObsVariant._
    import CopyNumberVariant._
      
      CopyNumberVariant(
        cnv.id.value,
        None,  // No Identifiers on CNV
        Observation.Status.Final,
        subject,
        CopyNumberVariant.Components(
          Chromosome(cnv.chromosome.value),
          StartRange(
            LBoundedRange(cnv.startRange.start.toDouble,cnv.startRange.end.map(_.toDouble))
          ),
          EndRange(
            LBoundedRange(cnv.endRange.start.toDouble,cnv.endRange.end.map(_.toDouble))
          ),
          CopyNumber(cnv.totalCopyNumber),
          RelativeCopyNumber(
            SimpleQuantity(cnv.relativeCopyNumber)
          ),
          cnv.cnA.map(SimpleQuantity(_)).map(CnA(_)),
          cnv.cnB.map(SimpleQuantity(_)).map(CnB(_)),
          cnv.reportedAffectedGenes
             .getOrElse(List.empty)
             .mapToF[CodeableConceptDynamic]
             .map(ReportedAffectedGene(_)),
          cnv.reportedFocality.map(ReportedFocality(_)),
          CNVType(
            CodeableConceptStatic(
              CodingStatic[dtos.CNV.Type.Value](
                cnv.`type`.toString,
                None,
                None
              )
            )
          ),
          cnv.copyNumberNeutralLoH
             .getOrElse(List.empty)
             .mapToF[CodeableConceptDynamic]
             .map(CopyNumberNeutralLoH(_)),
        )
      )
  }  

  implicit def cnvFromFHIR: CopyNumberVariant => dtos.CNV = {
    cnv =>

      import dtos.CNV._

      dtos.CNV(
        dtos.Variant.Id(cnv.id),
        dtos.Chromosome(cnv.component.chromosome.valueString),
        dtos.Variant.StartEnd(
          cnv.component.startRange.valueRange.low.value.toLong,
          cnv.component.startRange.valueRange.high.map(_.value.toLong),
        ),
        dtos.Variant.StartEnd(
          cnv.component.endRange.valueRange.low.value.toLong,
          cnv.component.endRange.valueRange.high.map(_.value.toLong),
        ),
        cnv.component.copyNumber.valueInteger,
        cnv.component.relativeCopyNumber.valueQuantity.value,
        cnv.component.cnA.map(_.valueQuantity.value),
        cnv.component.cnB.map(_.valueQuantity.value),
        Some(cnv.component.reportedAffectedGenes.map(_.valueCodeableConcept.mapTo[dtos.Gene.Coding])),
        cnv.component.reportedFocality.map(_.valueString),
        dtos.CNV.Type.withName(
          cnv.component.cnvType.valueCodeableConcept.coding.head.code
        ),
        Some(cnv.component.copyNumberNeutralLoH.map(_.valueCodeableConcept.mapTo[dtos.Gene.Coding]))
      )
  }


  implicit val ngsReportToFHIR: dtos.SomaticNGSReport => SomaticNGSReport = {

    ngsReport =>

      import ExtMetaData._

      implicit val subject  = LogicalReference[MTBPatient](ngsReport.patient)
      implicit val specimen = LogicalReference[TumorSpecimen](ngsReport.specimen)

      val tcc      = ngsReport.tumorCellContent.mapToF[ObsTumorCellContent]
      val tmb      = ngsReport.tmb.mapTo[ObsTMB]
      val msi      = ngsReport.msi.mapToF[ObsMSI]
      val brcaness = ngsReport.brcaness.mapToF[ObsBRCAness]
      val snvs     = ngsReport.simpleVariants.getOrElse(List.empty).mapToF[SimpleVariant]
      val cnvs     = ngsReport.copyNumberVariants.getOrElse(List.empty).mapToF[CopyNumberVariant]

      SomaticNGSReport(
        NonEmptyList.one(Identifier(ngsReport.id.value)),
        ngsReport.issueDate,
        DiagnosticReport.Status.Final,
        SomaticNGSReport.Extensions(
          ExtSequencingType(CodingStatic[dtos.SomaticNGSReport.SequencingType](ngsReport.sequencingType.value,None,None)),
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
        subject,
        NonEmptyList.one(specimen),
        NonEmptyList.of(
          Reference.contained(tmb),
        ) ++
          tcc.map(Reference.contained(_)).toList ++
          msi.map(Reference.contained(_)).toList ++
          brcaness.map(Reference.contained(_)).toList ++
          snvs.map(Reference.contained(_)) ++
          cnvs.map(Reference.contained(_)),
        SomaticNGSReport.Results(
          tcc,
          tmb,
          msi,
          brcaness,
          snvs,
          cnvs
        )
      )

  }


  implicit val ngsReportFromFHIR: SomaticNGSReport => dtos.SomaticNGSReport = {

    report =>
       
     import dtos.SomaticNGSReport._

     dtos.SomaticNGSReport(
       Id(report.identifier.head.value),
       report.subject.identifier,
       report.specimen.head.identifier,
       report.issued,
       SequencingType(report.extension.sequencingType.value.code),
       report.extension.metadata.map(
         m =>
           MetaData(
             m.kitType.value,
             m.kitManufacturer.value,
             m.sequencer.value,
             dtos.ReferenceGenome(m.refGenome.value),
             m.pipeline.map(_.value)
           )
       ),
       report.contained.tumorCellContent.mapToF[dtos.TumorCellContent],
       report.contained.brcaness.map(
         obs => BRCAness(obs.valueQuantity.value)
       ),
       report.contained.msi.map(
         obs => MSI(obs.valueQuantity.value)
       ),
       TMB(report.contained.tmb.valueQuantity.value),
       Some(report.contained.simpleVariants.mapToF[dtos.SimpleVariant]),
       Some(report.contained.copyNumberVariants.mapToF[dtos.CNV]),
  None,
  None,
  None
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
            List(
              LoE(
                LoE.Grade(CodingStatic(loe.grading.code,None)),
                loe.addendums.getOrElse(Set.empty)
                  .map(add => LoE.Addendum(CodingStatic(add.code,None)))
              )
            )
          ),
          ContainedMedication(med),
          rec.priority.map(_.mapTo[MedicationRequest.Priority.Value]),
          MedicationRequest.Status.Unknown,          
          MedicationRequest.Intent.Proposal,
          rec.issuedOn,
          LogicalReference[MTBPatient](rec.patient),
          NonEmptyList.one(LogicalReference[Condition](rec.diagnosis)),
          Reference.contained(med),
          Some(
            rec.ngsReport.toList
              .map(id => LogicalReference[SomaticNGSReportProfile](Identifier(id.value))) ++
            rec.supportingVariants.getOrElse(List.empty)
              .map(id => LogicalReference[SomaticVariantProfile](Identifier(id.value)))
          )
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
          Some(rec.contained.medication.mapTo[List[dtos.Medication.Coding]]),
          rec.priority.map(_.mapTo[dtos.TherapyRecommendation.Priority.Value]),
          rec.extension.map { l =>
            val loe = l.head
            dtos.LevelOfEvidence(
              dtos.Coding(dtos.LevelOfEvidence.Grading.withName(loe.grade.value.code),None),
              Option(loe.addendums)
                .filterNot(_.isEmpty)
                .map(_.map(_.value.code))
                .map(_.map(dtos.LevelOfEvidence.Addendum.withName _))
                .map(_.map(dtos.Coding(_,None)))
            )
          },
          rec.supportingInformation.flatMap(
            _.find(_.`type` == Resource.Type[SomaticNGSReport].name)
             .map(ref => dtos.SomaticNGSReport.Id(ref.identifier.value))
          ),
          rec.supportingInformation.map(
            _.filter(_.`type` == Resource.Type[SomaticVariantProfile].name)
             .map(ref => dtos.Variant.Id(ref.identifier.value))
          )
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

  implicit val counsellingReqFromFHIR: CounsellingRequest => dtos.GeneticCounsellingRequest = {
    req =>
      dtos.GeneticCounsellingRequest(
        req.identifier.head,
        req.subject.identifier,
        req.authoredOn,
        req.note.head.text
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

  implicit val rebiopsyReqFromFHIR: RebiopsyRequest => dtos.RebiopsyRequest = {
    req =>
      dtos.RebiopsyRequest(
        req.identifier.head,
        req.subject.identifier,
        req.specimen.head.identifier,
        req.authoredOn
      )
  }


  implicit val histologyReevaluationReqToFHIR: dtos.HistologyReevaluationRequest => HistologyReevaluationRequest = {
    req =>

     HistologyReevaluationRequest(
       NonEmptyList.one(Identifier(req.id.value)),
       ServiceRequest.Status.Unknown,
       ServiceRequest.Intent.Proposal,
       LogicalReference[MTBPatient](req.patient),
       req.issuedOn,
       NonEmptyList.one(LogicalReference[TumorSpecimen](req.specimen))
     )
  }

  implicit val histologyReevaluationReqFromFHIR: HistologyReevaluationRequest => dtos.HistologyReevaluationRequest = {
    req =>
      dtos.HistologyReevaluationRequest(
        dtos.HistologyReevaluationRequest.Id(req.identifier.head.value),
        req.subject.identifier,
        req.specimen.head.identifier,
        req.authoredOn
      )
  }


  implicit def carePlanIdFromIdentifier(id: Identifier) =
    dtos.CarePlan.Id(id.value)

  implicit def carePlanIdToIdentifier(id: dtos.CarePlan.Id) =
    Identifier(id.value)


  implicit def carePlanToFHIR(
    implicit studyInclusionReqs: List[dtos.StudyInclusionRequest]
  ): dtos.CarePlan => MTBCarePlan = {

    cp => 

      MTBCarePlan(        
        NonEmptyList.one(cp.id.mapTo[Identifier]),
        CarePlan.Status.Unknown,
        CarePlan.Intent.Proposal,
        cp.issuedOn,
        LogicalReference[MTBPatient](cp.patient),
        NonEmptyList.one(LogicalReference[Diagnosis](cp.diagnosis)),
        cp.description,
        MTBCarePlan.Activities(
          (
            cp.recommendations.getOrElse(List.empty)
              .map(LogicalReference[TherapyRecommendation](_)) ++
            cp.geneticCounsellingRequest
              .map(LogicalReference[CounsellingRequest](_)) ++
            cp.rebiopsyRequests.getOrElse(List.empty)
              .map(LogicalReference[RebiopsyRequest](_))
          )
          .map(MTBCarePlan.RequestReference(_)),
          cp.noTargetFinding.map(
            nt =>
            MTBCarePlan.Activity(
              MTBCarePlan.NoTarget(
                CarePlan.Activity.Detail.Status.Completed,
                CodeableConceptDynamic(CodingDynamic("no-target",Some("No target"),"-",None))
              )
            )
          ),
          MTBCarePlan.Activity(
            MTBCarePlan.StudyInclusionRequests(
              CarePlan.Activity.Detail.Status.Scheduled,
              CodeableConceptDynamic(CodingDynamic("study-inclusion-requests",Some("Study Inclusion Requests"),"-",None)),
              studyInclusionReqs.map(
                req =>
                  MTBCarePlan.NCTStudyReference(
                    LogicalReference[ResearchStudy](
                      Identifier(
                        req.nctNumber.value,
                        Some(java.net.URI.create("https://clinicaltrials.gov/"))
                      )
                    )
                  )
               )
            )
          )
        )
      )

  }

  implicit def carePlanFromFHIR(
    implicit
    counsellingRequests: List[CounsellingRequest],
    rebiopsyRequests: List[RebiopsyRequest]
  ): MTBCarePlan => (dtos.CarePlan,List[dtos.StudyInclusionRequest]) = {

    cp =>

      val studyInclusionRequests =
        cp.activity
          .studyInclusionRequests
          .detail
          .extension
          .map(
            ext => 
              dtos.StudyInclusionRequest(
                dtos.StudyInclusionRequest.Id(rndID.toString),
                cp.subject.identifier,
                cp.addresses.head.identifier,
                dtos.NCTNumber(ext.value.identifier.value),
                cp.created
              )
          )

      val carePlan = 
        dtos.CarePlan(
          dtos.CarePlan.Id(cp.identifier.head.value),
          cp.subject.identifier,
          cp.addresses.head.identifier,
          cp.created,
          cp.description,
          cp.activity.noTarget.map(
            nt =>
              dtos.NoTargetFinding(
                cp.subject.identifier,
                cp.addresses.head.identifier,
                cp.created,
              )
          ),
          Some(
            cp.activity.requests
              .map(_.reference)
              .filter(
                ref =>
                  ref.`type` == Resource.Type[TherapyRecommendation].name
              )
              .map(_.identifier),
          ),
          cp.activity.requests
            .map(_.reference)
            .find(
              ref =>
                ref.`type` == Resource.Type[CounsellingRequest].name &&
                counsellingRequests.exists(_.identifier.head == ref.identifier)
            )
            .map(_.identifier),
          Some(
            cp.activity.requests
              .map(_.reference)
              .filter(
                ref =>
                  ref.`type` == Resource.Type[RebiopsyRequest].name &&
                  rebiopsyRequests.exists(_.identifier.head == ref.identifier)
              )
              .map(_.identifier)
          ),
          Some(studyInclusionRequests.map(_.id))
        )

    (carePlan,studyInclusionRequests)
  }

  //---------------------------------------------------------------------------
  // Claim / ClaimResponse mappings
  //---------------------------------------------------------------------------

  implicit val claimToFHIR: dtos.Claim => ClaimDTO = {

    claim =>

      ClaimDTO(
        NonEmptyList.one(Identifier(claim.id.value)),
        claim.issuedOn,
        CodeableConceptStatic(CodingStatic(Claim.Type.Institutional)),
        Claim.Use.Claim,
        CodeableConceptStatic(CodingStatic(ProcessPriority.Normal)),
        Claim.Status.Draft,
        LogicalReference[TherapyRecommendation](claim.therapy),
        LogicalReference[Patient](claim.patient),
        LogicalReference[Organization](Identifier("DUMMY")),  //TODO
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
        CodeableConceptStatic(CodingStatic(Claim.Type.Institutional)),
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
              CodeableConceptStatic(CodingStatic(th.notDoneReason.code.toString,None,None))
            ),
            note
          )
        }

        case th: dtos.StoppedTherapy => {

          val medication = th.medication.getOrElse(List.empty).mapTo[MTBMedication]

          StoppedMolecularTherapy(
            identifier,
            ContainedMedication(medication),
            basedOn,
            molTh.recordedOn,
            subject,
            Reference.contained(medication),
            ClosedPeriod(th.period.start,th.period.end),
            th.dosage.map(_.mapTo[DosageDensity]).map(List(_)),
            NonEmptyList.one(
              CodeableConceptStatic(CodingStatic(th.reasonStopped.code.toString,None,None))
            ),
            note
          )
        }

        case th: dtos.CompletedTherapy => {

          val medication = th.medication.getOrElse(List.empty).mapTo[MTBMedication]

          CompletedMolecularTherapy(
            identifier,
            ContainedMedication(medication),
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
            ContainedMedication(medication),
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
            Some(th.contained.medication.mapTo[List[dtos.Medication.Coding]]),
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
            Some(th.contained.medication.mapTo[List[dtos.Medication.Coding]]),
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
            Some(th.contained.medication.mapTo[List[dtos.Medication.Coding]]),
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
        CodeableConceptStatic(CodingStatic(resp.value.code,None)) 
      )
  }

  implicit val responseFromFHIR: ObsRECIST => dtos.Response = {
    resp =>
      dtos.Response(
        resp.identifier.head,
        resp.subject.identifier,
        resp.partOf.head.identifier,
        resp.effectiveDateTime,
        dtos.Coding(
          dtos.RECIST.withName(resp.valueCodeableConcept.coding.head.code),
          None
        )
      )
  }


/*
  implicit class FHIROps[T](val t: T) extends AnyVal
  {
    def toFHIR[R <: Resource](implicit f: T => R) = f(t)
  }
*/


  //---------------------------------------------------------------------------
  // MTB File mappings
  //---------------------------------------------------------------------------


  implicit val mtbFileToFHIR: dtos.MTBFile => MTBFileBundle = {

    mtbfile =>

      implicit def toBundleEntry[R <: Resource](r: R): EntryOf[R] = EntryOf(r)

      implicit val pat = mtbfile.patient

      implicit val studyInclusionReqs = mtbfile.studyInclusionRequests.getOrElse(List.empty)  
    
      MTBFileBundle(
        MTBFileBundle.Entries(
          mtbfile.patient.mapTo[MTBPatient],
          mtbfile.episode.mapTo[MTBEpisode],
          mtbfile.consent.mapTo[BwHCConsent],
          mtbfile.diagnoses.getOrElse(List.empty).map(_.mapTo[Diagnosis]),
          mtbfile.familyMemberDiagnoses.getOrElse(List.empty).map(_.mapTo[FamilyMemberHistoryDTO]),
          mtbfile.previousGuidelineTherapies.getOrElse(List.empty).map(_.mapTo[PreviousGuidelineTherapy]),
          mtbfile.lastGuidelineTherapies.getOrElse(List.empty).map(_.mapTo[LastGuidelineTherapy]),
          mtbfile.ecogStatus.getOrElse(List.empty).map(_.mapTo[ObsECOG]),
          mtbfile.specimens.getOrElse(List.empty).map(_.mapTo[TumorSpecimen]),
          mtbfile.molecularPathologyFindings.getOrElse(List.empty).map(_.mapTo[MolecularPathologyReport]),
          mtbfile.histologyReports.getOrElse(List.empty).map(_.mapTo[HistologyReport]),
          mtbfile.ngsReports.getOrElse(List.empty).map(_.mapTo[SomaticNGSReport]),
          mtbfile.carePlans.getOrElse(List.empty).map(_.mapTo[MTBCarePlan]),
          mtbfile.recommendations.getOrElse(List.empty).map(_.mapTo[TherapyRecommendation]),
          mtbfile.geneticCounsellingRequests.getOrElse(List.empty).map(_.mapTo[CounsellingRequest]),
          mtbfile.rebiopsyRequests.getOrElse(List.empty).map(_.mapTo[RebiopsyRequest]),
//          mtbfile.histologyReevaluationRequests.getOrElse(List.empty).map(_.mapTo[HistologyReevaluationRequest]),
          mtbfile.claims.getOrElse(List.empty).map(_.mapTo[ClaimDTO]),
          mtbfile.claimResponses.getOrElse(List.empty).map(_.mapTo[ClaimResponseDTO]),
          mtbfile.molecularTherapies.getOrElse(List.empty).map(_.mapTo[MolecularTherapyHistory]),
          mtbfile.responses.getOrElse(List.empty).map(_.mapTo[ObsRECIST])
        )
      )
  }


  implicit val mtbFileFromFHIR: MTBFileBundle => dtos.MTBFile = {
    bundle =>

      val patient = bundle.entry.patient.mapTo[dtos.Patient]

      implicit val counsellingRequests = bundle.entry.geneticCounsellingRequests.map(_.resource)
      implicit val rebiopsyRequests    = bundle.entry.rebiopsyRequests.map(_.resource)

      val (carePlans,studyInclusionRequests) =
        bundle.entry.carePlans.mapToF[(dtos.CarePlan,List[dtos.StudyInclusionRequest])]
          .unzip


      dtos.MTBFile(
        patient,
        bundle.entry.consent.mapTo[dtos.Consent],
        bundle.entry.episode.mapTo[dtos.MTBEpisode],
        Some(bundle.entry.diagnoses.mapToF[dtos.Diagnosis]),
        Some(bundle.entry.familyMemberDiagnoses.mapToF[dtos.FamilyMemberDiagnosis]),
        Some(bundle.entry.previousGLTherapies.mapToF[dtos.PreviousGuidelineTherapy]),
        Some(bundle.entry.lastGLTherapies.mapToF[dtos.LastGuidelineTherapy]),
        Some(bundle.entry.ecogs.mapToF[dtos.ECOGStatus]),
        Some(bundle.entry.specimens.mapToF[dtos.Specimen]),
        Some(bundle.entry.molecularPathology.mapToF[dtos.MolecularPathologyFinding]),
        Some(bundle.entry.histology.mapToF[dtos.HistologyReport]),
        Some(bundle.entry.ngsReports.mapToF[dtos.SomaticNGSReport]),
        Some(carePlans),
        Some(bundle.entry.therapyRecommendations.mapToF[dtos.TherapyRecommendation]),
        Some(bundle.entry.geneticCounsellingRequests.mapToF[dtos.GeneticCounsellingRequest]),
        Some(bundle.entry.rebiopsyRequests.mapToF[dtos.RebiopsyRequest]),
//        Some(bundle.entry.histologyReevaluationRequests.mapToF[dtos.HistologyReevaluationRequest]),
    None, //TODO
        Some(studyInclusionRequests.flatten),
        Some(bundle.entry.claims.mapToF[dtos.Claim]),
        Some(bundle.entry.claimResponses.mapToF[dtos.ClaimResponse]),
        Some(bundle.entry.molecularTherapies.mapToF[dtos.MolecularTherapyDocumentation]),
        Some(bundle.entry.responses.mapToF[dtos.Response]),
      )

  }

}
