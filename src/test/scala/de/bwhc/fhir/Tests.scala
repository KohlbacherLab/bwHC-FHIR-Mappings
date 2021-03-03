package de.bwhc.fhir


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._

import play.api.libs.json.Json

import ca.uhn.fhir.context.FhirContext
import ca.uhn.fhir.parser.StrictErrorHandler
import ca.uhn.fhir.validation.{
  FhirValidator,
  ValidationResult
}


import de.ekut.tbi.generators.Gen

import de.bwhc.util.mapping.syntax._

import de.bwhc.mtb.data.entry.dtos
import de.bwhc.mtb.data.gens._

import org.hl7.fhir.r4.FHIRJson._
import Mappings._



class Tests extends AnyFlatSpec
{

  implicit val rnd = new scala.util.Random(42)


  var fhirContext = FhirContext.forR4
  fhirContext.setParserErrorHandler(new StrictErrorHandler)


  val hapiValidator =
    fhirContext.newValidator.setValidateAgainstStandardSchema(true)



  "Patient" must "be serialized to valid FHIR/JSON" in {
    
    val patient = Gen.of[dtos.Patient].map(_.mapTo[MTBPatient]).next

    val fhirJson = patient.toFHIRJson

    println(Json.prettyPrint(fhirJson))

    val validation = hapiValidator.validateWithResult(Json.stringify(fhirJson))
    if (!validation.isSuccessful) validation.getMessages.forEach(println)

    validation.isSuccessful mustBe true


    val parsed = fhirJson.asFHIR[MTBPatient]

    parsed.isSuccess mustBe true

  }


  "MTBFileBundle" must "be serialized to valid FHIR/JSON" in {
    
    val bundle = Gen.of[dtos.MTBFile].map(_.mapTo[MTBFileBundle]).next

    val fhirJson = bundle.toFHIRJson

    println(Json.prettyPrint(fhirJson))


    val validation = hapiValidator.validateWithResult(Json.stringify(fhirJson))
    if (!validation.isSuccessful) validation.getMessages.forEach(println)

    validation.isSuccessful mustBe true


    val parsed = fhirJson.asFHIR[MTBFileBundle]

    parsed.filter(_ == bundle).isSuccess mustBe true

  }



}

