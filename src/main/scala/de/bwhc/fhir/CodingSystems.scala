package de.bwhc.fhir



import org.hl7.fhir.r4.Coding.System

import de.bwhc.mtb.data.entry.dtos._
import de.bwhc.mtb.data.entry.dtos.{Medication => ATC}



object CodingSystems
{


  implicit val icd10gmSystem =
    System[ICD10GM]("http://fhir.de/CodeSystem/dimdi/icd-10-gm")


  implicit val icdO3tSystem =
    System[ICDO3T]("urn:oid:2.16.840.1.113883.6.43.1")


  implicit val icdO3mSystem =
    System[ICDO3M]("urn:oid:2.16.840.1.113883.6.43.1")
  

  implicit val whoCnsGradeSystem =
    System[WHOGrade.Value]("WHO-CNS-Tumor-Grading")


  implicit val diagTumorStatusSystem =
    System[Diagnosis.Status.Value]("MTB-Tumor-Status")


  implicit val ecogStatusSystem =
    System[ECOG.Value]("ECOG-Performance-Status")


  implicit val atcMedicationSystem =
    System[ATC]("http://fhir.de/CodeSystem/dimdi/atc")




}
