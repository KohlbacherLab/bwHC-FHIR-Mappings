package de.bwhc.fhir



import org.hl7.fhir.r4.CodingSystem

import de.bwhc.mtb.data.entry.dtos._
import de.bwhc.mtb.data.entry.dtos.{Medication => ATC}



object CodingSystems
{


  implicit val icd10gmSystem =
    CodingSystem[ICD10GM]("http://fhir.de/CodeSystem/dimdi/icd-10-gm")


  implicit val icdO3tSystem =
    CodingSystem[ICDO3T]("urn:oid:2.16.840.1.113883.6.43.1")


  implicit val icdO3mSystem =
    CodingSystem[ICDO3M]("urn:oid:2.16.840.1.113883.6.43.1")
  

  implicit val whoCnsGradeSystem =
    CodingSystem[WHOGrade.Value]("WHO-CNS-Tumor-Grading")


  implicit val diagTumorStatusSystem =
    CodingSystem[Diagnosis.Status.Value]("MTB-Tumor-Status")


  implicit val ecogStatusSystem =
    CodingSystem[ECOG.Value]("ECOG-Performance-Status")


//  implicit val atcMedicationSystem =
//    CodingSystem[ATC]("http://fhir.de/CodeSystem/dimdi/atc")


  implicit val recistSystem =
    CodingSystem[RECIST.Value]("RECIST")


  implicit val system =
    CodingSystem[SomaticNGSReport.SequencingType]("http://bwhc.de/mtb/somatic-ngs-report/sequencing-type")


}
