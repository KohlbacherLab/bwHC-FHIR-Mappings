package de.bwhc.fhir


import java.time.LocalDate

import cats.data.NonEmptyList

import org.hl7.fhir.r4
import org.hl7.fhir.r4._
import org.hl7.fhir.r4.Medication._

import org.hl7.fhir.r4.json._

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.{Medication => ATC}


import CodingSystems._


trait MTBMedicationProfile
extends Medication
   with Medication.id[Required]
   with Medication.ingredient[
     Medication.IngredientElement[CodeableConcept],
     Required
   ]
//   with Medication.ingredientNel[
//     Medication.IngredientElement[CodeableConcept]
//   ]


final case class MTBMedication
(
  id: String,
//  ingredient: NonEmptyList[MTBMedication.Ingredient]
  ingredient: List[MTBMedication.Ingredient]
)
extends MTBMedicationProfile



object MTBMedication
{

  implicit val profiles =
    Meta.Profiles[MTBMedication]("http://bwhc.de/mtb/medication")
    
  final case class Ingredient
  (
    itemCodeableConcept: BasicCodeableConcept[ATC]
  )
  extends Medication.IngredientElement[CodeableConcept]
     with Medication.Ingredient.itemCodeableConcept[BasicCodeableConcept[ATC]]


  implicit val formatIngredient = Json.format[Ingredient]

  implicit val format = Json.format[MTBMedication]
  
}
