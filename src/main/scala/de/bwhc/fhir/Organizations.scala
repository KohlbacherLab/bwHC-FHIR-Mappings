package de.bwhc.fhir


import java.net.URI

import org.hl7.fhir.r4.{
  Identifier,
  Organization
}



sealed trait ZPM extends Organization

sealed trait HealthInsurance extends Organization


object HealthInsurance
{


  val AOK =
    Identifier(
      value = "107815772",
      system = Some(URI.create("https://www.dguv.de/arge-ik"))
    )

  val Barmer =
    Identifier(
      value = "IK_Barmer",
      system = Some(URI.create("https://www.dguv.de/arge-ik"))
    )


/*
  val AOK =
    Identifier(
      value = "12345678987654321",
      system = None
    )

  val Barmer =
    Identifier(
      value = "12345678987654321",
      system = None
    )
*/
}

