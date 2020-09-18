package de.bwhc.fhir



import cats.data.NonEmptyList

import org.hl7.fhir.r4._
import org.hl7.fhir.r4.json._

import play.api.libs.json.Json



trait FamilyMemberHistoryProfile
  extends FamilyMemberHistory
  with FamilyMemberHistory.identifierNel



final case class FamilyMemberHistoryDTO
(
  identifier: NonEmptyList[Identifier],
  status: FamilyMemberHistory.Status.Value,
  patient: Reference[MTBPatient],
  relationship: BasicCodeableConcept[HL7v3FamilyMember.Value]
)
extends FamilyMemberHistoryProfile



object FamilyMemberHistoryDTO
{

  implicit val profiles =
    Meta.Profiles[FamilyMemberHistoryDTO]("http://bwhc.de/mtb/family-member-history")


  implicit val format = Json.format[FamilyMemberHistoryDTO]

}
