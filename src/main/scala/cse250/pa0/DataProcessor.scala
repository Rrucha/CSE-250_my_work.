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

    var list1: List[Int] = List()

    var luck: Int = 0
  var Index: Int =0


    var Acc: Array[String] = Array()
    var Answer: Array[String] = Array()
    val len : Int = splitHeaderRow.length



      for (i <- splitHeaderRow){

        if (i.contains('"')){
          // val indexofi = splitHeaderRow.indexOf(i)

          if (i(0) == '"' && luck ==0){
            firstquote = i
            indexoffirstquote = Index
            list1 = list1 :+ indexoffirstquote
            luck = 2
          }


          if (i(i.length-1) == '"' && luck ==2) {
            lastquote = i
            indexoflastquote = Index
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

            oof4 = oof3.replaceFirst(",\"","")
            oof4 =  oof4.replace("\"\"","\"")
            oof = oof4.lastIndexOf("\"")
            val splits = oof4.splitAt(oof)
            oof5 = splits._1 + splits._2.replaceFirst("\"","")
            Acc = Acc :+ oof5
            luck = 0
          }

        }
        else if (luck != 2){
          Acc = Acc :+ i
        }
        Index = Index + 1
    }

    if(Acc.length < 31){
      while(Acc.length < 31){
        Acc = Acc :+ ","
      }
      Acc
    }
    else if (Acc.length >31){
      while(Acc.length > 31){
        Acc = Acc.drop(1)
      }
      Acc
    }
    else{
      Acc
    }
  }



  def rowArrayToSolarInstallation(rowData: Array[String]): SolarInstallation = {
    val acc: SolarInstallation = new SolarInstallation
    var dd: mutable.Map[String, String] = new mutable.HashMap[String, String]
    val head = SolarInstallation.HEADERS
    val req_head = SolarInstallation.REQUIRED_HEADERS

    for (i <- head.indices) {


      if  (req_head.contains(head(i))) {
        val keys: String = head(i)
        val values: String = rowData(i)

        acc.fields += (keys -> values)


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

    val data = dataset.drop(1)
    var acc: Float = 0.0f
    for (k <- data) {
      for(key <- k.fields.keys) {
        if (key == "EXPECTED_KWH_ANNUAL_PRODUCTION" ){

          if (k.fields(key) == "" || k.fields(key).toFloat <= 0){
           acc = acc
          }
          else{
            acc =  k.fields(key).toFloat + acc
          }

        }
      }
    }
  acc
  }




}
