package de.bwhc.fhir


import java.time.YearMonth

import scala.util.Try

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Patient._
import org.hl7.fhir.r4.json._

import play.api.libs.json.{Json,Format,Reads,Writes,JsString,JsSuccess,JsError}


trait MTBPatientProfile
extends Patient
   with Patient.identifierNel
   with Patient.gender[Required]
   with Patient.birthDate[YearMonth,Optional]
   with Patient.managingOrganization[Optional]
   with Patient.deceasedDateTime[YearMonth,Optional]
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
  birthDate: Option[YearMonth],
  deceasedDateTime: Option[YearMonth],
  managingOrganization: Option[LogicalReference[ZPM]],
  contact: Option[List[MTBPatient.HealthInsuranceContact]]
) extends MTBPatientProfile


object MTBPatient
{

  implicit val profiles = Meta.Profiles[MTBPatient]("http://bwhc.de/mtb/patient")


  case class HealthInsuranceContact(
    organization: LogicalReference[HealthInsurance],
    relationship: NonEmptyList[BasicCodeableConcept[Contact.RelationshipType.Value]] =
      NonEmptyList.of(BasicCodeableConcept(BasicCoding(Contact.RelationshipType.I)))
  ) extends Patient.ContactElement
       with Patient.Contact.organization[Required]
       with Patient.Contact.relationshipNel

    
  implicit val formatContact = Json.format[HealthInsuranceContact]



  import java.time.format.DateTimeFormatter

  private val yyyyMM = DateTimeFormatter.ofPattern("yyyy-MM")
 
  implicit val formatYearMonth: Format[YearMonth] =
    Format(
      Reads(
        js =>
          for {
            s <- js.validate[String]
            result <-
              Try(YearMonth.parse(s,yyyyMM))
                .map(JsSuccess(_))
                .getOrElse(JsError(s"Invalid Year-Month value $s; expected format YYYY-MM") )
          } yield result
      ),
      Writes(
        d => JsString(yyyyMM.format(d))
      )
    )


//  implicit val formatPatient = FHIRJson.format[MTBPatient]
  implicit val formatPatient = Json.format[MTBPatient]
  
}

