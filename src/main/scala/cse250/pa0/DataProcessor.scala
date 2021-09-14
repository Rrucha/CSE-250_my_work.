package cse250.pa0
/**
 * cse250.pa0.AssessmentDataProcessor
 *
 * Copyright 2021 Oliver Kennedy (okennedy@buffalo.edu)
 *           2021 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: Rruchasi
 * Person#: 50355134
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */




import cse250.objects.SolarInstallation

import scala.collection.mutable



object DataProcessor {

  def splitArrayToRowArray(splitHeaderRow: Array[String]): Array[String]={

    var firstquote: String =""
    var indexoffirstquote : Int = -1
    var lastquote: String= ""
    var indexoflastquote: Int = -1
    var ListofA: List[String] = List()
    var ListofindexofA: List[Int] = List()
    var list1: List[Int] = List()

    var luck: Int = -1

    var firstquote2: String =""
    var indexoffirstquote2 : Int = -1
    var lastquote2: String= ""
    var indexoflastquote2: Int = -1

    var Acc: Array[String] = Array()
    var Answer: Array[String] = Array()
    val len : Int = splitHeaderRow.length
  if (splitHeaderRow.length == 31) {
    splitHeaderRow.map(_.trim)
  }

    else{
    for (i <- splitHeaderRow){
      val ind : Int = splitHeaderRow.indexOf(i)
     if (i.contains('"')){
       // val indexofi = splitHeaderRow.indexOf(i)

        if (i(0) == '"'){
          firstquote = i
          indexoffirstquote = splitHeaderRow.indexOf(firstquote)
          list1 = list1 :+ indexoffirstquote
          luck = 2
        }


        if (i(i.length-1) == '"') {
          lastquote = i
          indexoflastquote = splitHeaderRow.indexOf(lastquote)
          // val oof = splitHeaderRow(indexofi-1) + "," + splitHeaderRow(indexofi)
          //  val oof2 = oof.substring(2, oof.length() - 1)
          var oof3 = ""
          var oof = 0
          var oof4 = ""
          var oof5 = ""
          for (a <- Range(indexoffirstquote, (indexoflastquote + 1))) {
            //println(splitHeaderRow(a))

            oof3 = oof3 + "," + splitHeaderRow(a)

          }

          oof3 = oof3.replace(",\"","")
          oof4 =  oof3.replace("\"\"","\"")
          oof = oof4.lastIndexOf("\"")
          val splits = oof4.splitAt(oof)
        oof5 = splits._1 + splits._2.drop(1)
          Acc = Acc :+ oof5
          luck = 0
        }

      }
     else if (luck != 2){
       Acc = Acc :+ i
        }
    }
    Acc
    }
  }



  def rowArrayToSolarInstallation(rowData: Array[String]): SolarInstallation = {
    val acc: SolarInstallation = new SolarInstallation
    var dd: mutable.Map[String, String] = new mutable.HashMap[String, String]
    val head = SolarInstallation.HEADERS
    val req_head = SolarInstallation.REQUIRED_HEADERS

    for (i <- head.indices) {


          if (head(i) != "LEGACY_PROJECT_NUMBER" && head(i) != "PROGRAM_TYPE" && head(i) != "ELECTRIC_UTILITY" && head(i) !=  "SOLICITATION" && head(i) != "REMOTE_NET_METERING" && head(i) != "GEOREFERENCE") {
            val keys: String = head(i)
            val values: String = rowData(i)

            dd = mutable.HashMap(keys -> values)
            acc.fields = acc.fields ++ dd


        }
    }


    acc
  }

  def computeUniqueInverterManufacturers(dataset: Array[SolarInstallation]): Int = {
    var acc: Int = 0
    val data = dataset.drop(1)

    val Checking1 = new Array[String](data.length)
    val Checking = new Array[String](data.length)
    for (k <- Range(0,data.length-1)) {
     for (key <- data(k).fields.keys) {

          Checking1(k) =  key
          if (key == "PRIMARY_INVERTER_MANUFACTURER" ){

            if (!Checking.contains(data(k).fields(key)) && data(k).fields(key) != ""){
              Checking(k) = data(k).fields(key)
              acc = acc + 1
            }
          }
        }
       }


    acc
  }

  def computeTotalExpectedKWHAnnualProduction(dataset: Array[SolarInstallation]): Float = {
    var acc: Float = 0
    val data = dataset.drop(1)
    for (k <- Range(0,data.length-1)) {
      for(key <- data(k).fields.keys) {
        if (key == "EXPECTED_KWH_ANNUAL_PRODUCTION" ){

          if (data(k).fields(key) == "" || data(k).fields(key).toFloat < 0){

          }
          else{
            acc = acc + data(k).fields(key).toFloat
          }

        }
      }
    }
  acc
  }


  def main(args: Array[String]): Unit = {

     val acc =splitArrayToRowArray("08/31/2021,0000258483,,Jamacia,Queens,NY,11420,Residential,Residential/Small Commercial,PON 2112,Consolidated Edison,Power Purchase Agreement,08/15/2020,12/18/2020,Complete,\"IGS Solar, LLC\",Enphase Energy Inc.,IQ7PLUS-72-x-US-& [240V],10,LG Electronics Inc.,LG350N1C-V5,10,14245.00,700.00,3.50,3734.00,,No,No,No,POINT (-73.815675 40.673382)".split(','))

    val SECOND_ROW = "07/31/2021,0000000276,01001-00018,Ithaca,Tompkins,NY,14850,Non-Residential,Residential/Small Commercial,PON 1184,,,08/02/2003,03/11/2005,Complete,\"Solar Works, Inc.\",Fronius USA,IG 2500-LV POS,1,Sharp,NE-165U5,12,,,1.98,,,No,No,No,POINT (-76.497069 42.443738)"
    val SECOND_ROW2 = "07/31/2021,0000000276,01001-00018,Ithaca,Tompkins,NY,14850,Non-Residential,Residential/Small Commercial,PON 1184,,,08/02/2003,03/11/2005,Complete,\"Solar Works, Inc.\",,IG 2500-LV POS,1,Sharp,NE-165U5,12,,,1.98,,,No,No,No,POINT (-76.497069 42.443738)"

    val splitSecondRow = SECOND_ROW.split(',')
    val splitSecondRow2 = SECOND_ROW2.split(',')
    val rowArray = DataProcessor.splitArrayToRowArray(splitSecondRow)
    val rowArray2 = DataProcessor.splitArrayToRowArray(splitSecondRow2)
     val new1: Array[SolarInstallation] = Array( rowArrayToSolarInstallation(acc),rowArrayToSolarInstallation(rowArray),rowArrayToSolarInstallation(rowArray2))
   // println( computeUniqueInverterManufacturers(new1) )
   //   println(computeTotalExpectedKWHAnnualProduction(new1))


    val testData = "First Cell,Second Cell,\"\"\"Best\"\" the, kool \"\"Best\"\" Around\",\"Comma, Cell\",\"\"\"Object-Orientation, Abstraction, and Data Structures Using Scala\"\"\""
    val Splittest = testData.split(',')
    val row = splitArrayToRowArray(Splittest)





  }

}
