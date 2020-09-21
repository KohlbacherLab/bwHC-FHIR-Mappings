package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.EpisodeOfCare._

import play.api.libs.json.Json



trait MTBEpisodeProfile
extends EpisodeOfCare
   with EpisodeOfCare.identifierNel
   with EpisodeOfCare.period[Required]


final case class MTBEpisode
(
  identifier: NonEmptyList[Identifier],
  status: EpisodeOfCare.Status.Value,
  patient: LogicalReference[MTBPatient],
  period: OpenEndPeriod[LocalDate]
)
extends MTBEpisodeProfile


object MTBEpisode
{

  implicit val profiles = Meta.Profiles[MTBEpisode]("http://bwhc.de/mtb/episode")
    

  import org.hl7.fhir.r4.json._

    
  implicit val format = Json.format[MTBEpisode]
  
}
