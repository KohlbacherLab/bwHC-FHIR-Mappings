package de.bwhc.fhir



import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Claim._
import org.hl7.fhir.r4.ClaimResponse._

import play.api.libs.json.Json



abstract class ClaimProfile
extends Claim
   with Claim.identifierNel
   with Claim.created[LocalDate]
   with Claim.prescription[MedicationRequest,Required]
   with Claim.provider[Organization]

trait ClaimProfileDefs
{
  implicit def profiles[C <: ClaimProfile] =
    Meta.Profiles[C]("http://bwhc.de/mtb/claim")
}


abstract class ClaimResponseProfile
extends ClaimResponse
   with ClaimResponse.identifierNel
   with ClaimResponse.created[LocalDate]
   with ClaimResponse.request[Required]

trait ClaimResponseProfileDefs
{
  implicit def profiles[C <: ClaimResponseProfile] =
    Meta.Profiles[C]("http://bwhc.de/mtb/claim-response")
}





final case class ClaimDTO
(
  identifier: NonEmptyList[Identifier],  
  created: LocalDate,
  `type`: CodeableConceptStatic[Claim.Type.Value],
//  `type`: BasicCodeableConcept[Claim.Type.Value],
  use: Claim.Use.Value,
  priority: CodeableConceptStatic[ProcessPriority.Value],
//  priority: BasicCodeableConcept[ProcessPriority.Value],
  status: Claim.Status.Value,
  prescription: LogicalReference[TherapyRecommendation],
  patient: LogicalReference[Patient],
  provider: LogicalReference[Organization]
)
extends ClaimProfile

object ClaimDTO extends ClaimProfileDefs
{

  import org.hl7.fhir.r4.json._

  implicit val format = Json.format[ClaimDTO]
}



final case class ClaimResponseDTO
(
  identifier: NonEmptyList[Identifier],  
  created: LocalDate,
  `type`: CodeableConceptStatic[Claim.Type.Value],
//  `type`: BasicCodeableConcept[Claim.Type.Value],
  use: Claim.Use.Value,
  status: Claim.Status.Value,
  patient: LogicalReference[Patient],
  request: LogicalReference[Claim],
  insurer: LogicalReference[Organization],
  outcome: ClaimResponse.Outcome.Value
)
extends ClaimResponseProfile

object ClaimResponseDTO extends ClaimResponseProfileDefs
{

  import org.hl7.fhir.r4.json._

  implicit val format = Json.format[ClaimResponseDTO]
}








