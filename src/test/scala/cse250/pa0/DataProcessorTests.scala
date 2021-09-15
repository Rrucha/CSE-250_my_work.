/**
 * AssessmentDataProcessorTests.scala
 *
 * Copyright 2021 Oliver Kennedy (okennedy@buffalo.edu)
 *           2021 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.pa0

import cse250.objects.SolarInstallation
import cse250.pa0.DataProcessor.splitArrayToRowArray
import org.scalatest.flatspec.AnyFlatSpec



class DataProcessorTests extends AnyFlatSpec {
  behavior of "DataProcessor.splitArrayToRowArray"
  it should "return an Array with 32 entries when processing the header row" in {
    val headerRow = SolarInstallation.HEADERS.mkString(",")
    val splitHeaderRow = headerRow.split(',')
    val result = DataProcessor.splitArrayToRowArray(splitHeaderRow)
    assert(result.length == 31)
    for (i <- splitHeaderRow.indices) assert(splitHeaderRow(i) == result(i))
  }


  it should "produces an array of correct length when processing the first entry (2nd row) of dataset" in {
    val splitSecondRow = SECOND_ROW.split(',')
    val result = DataProcessor.splitArrayToRowArray(splitSecondRow)
    assert(result.length == 31)
    for (i <- EXPECTED_SECOND_ROW.indices) assert(result(i) == EXPECTED_SECOND_ROW(i))
  }

  behavior of "AssessmentDataProcessor.rowArrayToSolarInstallation"
  it should "return an exactly the required header fields" in {
    val headerRow = SolarInstallation.HEADERS.mkString(",")
    val splitHeaderRow = headerRow.split(',')
    val rowArray = DataProcessor.splitArrayToRowArray(splitHeaderRow)
    val result = DataProcessor.rowArrayToSolarInstallation(rowArray)
    assert(result.fields.size == SolarInstallation.REQUIRED_HEADERS.length)
    assert(result.toString == SolarInstallation.REQUIRED_HEADERS.mkString("",",",""))
  }

  it should "correctly process the first entry (2nd row) of file" in {
    val splitSecondRow = SECOND_ROW.split(',')
    val rowArray = DataProcessor.splitArrayToRowArray(splitSecondRow)
    assert(rowArray.length == 31)
    val result = DataProcessor.rowArrayToSolarInstallation(rowArray)
    assert(result.fields.size == SolarInstallation.REQUIRED_HEADERS.length)
    val expectedToString = EXPECTED_SECOND_ROW_REQUIRED.mkString(",")
    assert(result.toString == expectedToString)
  }

  val SECOND_ROW = "07/31/2021,0000000276,01001-00018,Ithaca,Tompkins,NY,14850,Non-Residential,Residential/Small Commercial,PON 1184,,,08/02/2003,03/11/2005,Complete,\"Solar Works, Inc.\",Fronius USA,IG 2500-LV POS,1,Sharp,NE-165U5,12,,,1.98,,,No,No,No,POINT (-76.497069 42.443738)"
  val EXPECTED_SECOND_ROW = Array(
    "07/31/2021",
    "0000000276",
    "01001-00018",
    "Ithaca",
    "Tompkins",
    "NY",
    "14850",
    "Non-Residential",
    "Residential/Small Commercial",
    "PON 1184",
    "",
    "",
    "08/02/2003",
    "03/11/2005",
    "Complete",
    "Solar Works, Inc.",
    "Fronius USA",
    "IG 2500-LV POS",
    "1",
    "Sharp",
    "NE-165U5",
    "12",
    "",
    "",
    "1.98",
    "",
    "",
    "No",
    "No",
    "No",
    "POINT (-76.497069 42.443738)"
  )
  val EXPECTED_SECOND_ROW_REQUIRED = Array(
    "07/31/2021",
    "0000000276",
    "Ithaca",
    "Tompkins",
    "NY",
    "14850",
    "Non-Residential",
    "",
    "08/02/2003",
    "03/11/2005",
    "Complete",
    "Solar Works, Inc.",
    "Fronius USA",
    "IG 2500-LV POS",
    "1",
    "Sharp",
    "NE-165U5",
    "12",
    "",
    "",
    "1.98",
    "",
    "No",
    "No",
    "No"
  )

  val failedline1 = "08/31/2021,0000196776,,\"Cooperstown,\",Otsego,NY,13326,Residential,Residential/Small Commercial,PON 2112,NYS Electric and Gas,Purchase,07/16/2019,09/03/2019,Complete,Kasselman Solar LLC,Enphase Energy Inc.,IQ7PLUS-72-x-US [240V],6,LG Electronics Inc.,LG365Q1C-A5,6,7506.00,766.00,2.19,1958.00,,No,No,No,POINT (-74.908187 42.717237)".split(",")
  val failedline = "Complete,\"Solar Alchemy, Inc.\",\"OutBack Power Technologies, Inc.\",GS4048A,1,Hanwha Q CELLS,Q.PEAK DUO-G8+ 340,15,29440.00,1785.00,5.10,5229.00,,No,No,No,POINT (-74.946085 41.838807)".split(",")
  val we =  DataProcessor.splitArrayToRowArray(failedline1)
  val wee =  DataProcessor.splitArrayToRowArray(failedline)

  assert(we(0)== "08/31/2021","failedline failed1")
  assert(we(1)=="0000196776","failedline failed1")
  assert(we(2)=="","failedline failed1")
  assert(we(3)=="Cooperstown,","failedline failed1")


  assert(wee(0)== "Complete","failedline failed")
  assert(wee(1)=="Solar Alchemy, Inc.","failedline failed")
  assert(wee(2)=="OutBack Power Technologies, Inc.","failedline failed")
  assert(wee(3)=="GS4048A","failedline failed")

  val testData = "First Cell,Second Cell,\"\"\"Best\"\" the, kool \"\"Best\"\" Around\",\"Comma, Cell\",\"\"\"Object-Orientation, Abstraction, and Data Structures Using Scala\"\"\""
  val Splittest = testData.split(',')

  val row = splitArrayToRowArray(Splittest)
  println("**************")
  assert(row(0)=="First Cell","First Cell")
  assert(row(1)=="Second Cell","Second Cell")
  assert(row(2)=="\"Best\" the, kool \"Best\" Around","\"Best\" the, kool \"Best\" Around")
  assert(row(3)=="Comma, Cell","Comma, Cell")
  assert(row(4)=="\"Object-Orientation, Abstraction, and Data Structures Using Scala\"","\"Object-Orientation, Abstraction, and Data Structures Using Scala\"")

}

