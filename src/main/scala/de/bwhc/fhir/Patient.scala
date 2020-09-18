package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Patient._
import org.hl7.fhir.r4.json._

import play.api.libs.json.Json


trait MTBPatientProfile
extends Patient
   with Patient.identifierNel
   with Patient.gender[Required]
   with Patient.birthDate[Optional]
   with Patient.managingOrganization[Optional]
   with Patient.deceasedDateTime[LocalDate,Optional]
   with Patient.contact[
     Patient.ContactElement
       with Patient.Contact.organization[Required]
       with Patient.Contact.relationshipNel,
     Optional
   ]



final case class MTBPatient
(
  identifier: NonEmptyList[Identifier],
  gender: AdministrativeGender.Value,
  birthDate: Option[LocalDate],
  deceasedDateTime: Option[LocalDate],
  managingOrganization: Option[Reference[ZPM]],
  contact: Option[List[MTBPatient.HealthInsuranceContact]]
) extends MTBPatientProfile


object MTBPatient
{

  implicit val profiles = Meta.Profiles[MTBPatient]("http://bwhc.de/mtb/patient")


  case class HealthInsuranceContact(
    organization: Reference[HealthInsurance],
    relationship: NonEmptyList[BasicCodeableConcept[Contact.RelationshipType.Value]] =
      NonEmptyList.of(BasicCodeableConcept(BasicCoding(Contact.RelationshipType.I)))
  ) extends Patient.ContactElement
       with Patient.Contact.organization[Required]
       with Patient.Contact.relationshipNel

    
  implicit val formatContact = Json.format[HealthInsuranceContact]

  implicit val formatPatient = Json.format[MTBPatient]
  
}

