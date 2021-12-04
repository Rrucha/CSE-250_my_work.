/**
 * cse250.pa3.Join.scala
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
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
 package cse250.pa3

import cse250.objects.{HealthRecord, HealthRecordAttribute, HealthRecordBirthday, HealthRecordZipCode, VoterRecord}

import java.io.File
import java.text.DateFormat
import java.util.Date
import scala.io.Source
import scala.reflect.internal.NoPhase.assignsFields.||
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
object DataTools {
  /**
   * Convert date string (e.g., MM/DD/YYYY) to a [[Date]]
   * @param    dateString     A date string in a standard format like MM/DD/YYYY
   * @return                  A [[Date]] encoding dateString
   */
  def parseDate(dateString: String): Date = 
    DateFormat.getDateInstance(DateFormat.SHORT).parse(dateString)


  /**
   * Load a sequence of "anonymized" [[HealthRecord]]s from a CSV file
   * @param    filename    The path to a CSV file.
   * @return               The [[HealthRecord]] objects loaded from the file.
   * 
   * This function should make the following assumptions about the CSV file:
   * 1. The first line of the CSV file is a header.  
   * 2. Header fields are 
   *     * "Birthday"
   *     * "Zip Code"
   *     * "Wear Glasses?"
   *     * "Allergic to Dogs?"
   *     * "Brown Hair?"
   *     * "Blue Eyes?"
   * 3. Every subsequent line may be split into fields with [[String]]'s split method
   * 4. Columns containing dates are in a format interpretable by [[parseDate]]
   * 
   * Examples of valid CSV files can be found in `src/test/resources/`
   */
    def BOOL (input : String): Boolean ={
      if (input == "Yes" ||  input == "yes"){
        true
      }
      else (
        false
      )
    }
  def loadHealthRecords(filename: File): Seq[HealthRecord] = {
       val inputFile = Source.fromFile(filename)
       val lines = inputFile.getLines()
       lines.drop(1)
       var ans : Seq[HealthRecord] = Seq()
        for (line <- lines) {
          val rowData = line.split(",",6)
      //    var birthday: Date = parseDate(rowData(0))
            var birthday: Date = null
         if (rowData(0).nonEmpty) {
            birthday = parseDate(rowData(0))
        }
          var zipcode  : String = null
          if (rowData(1).nonEmpty) {
            zipcode   = rowData(1)
          }
          val glasses  : Boolean = BOOL(rowData(2))
          val dog : Boolean =   BOOL(rowData(3))
          val hair: Boolean =   BOOL(rowData(4))
          val eyes : Boolean =   BOOL(rowData(5))
          val add = HealthRecord(birthday,zipcode,glasses,dog,hair,eyes)
          ans = ans :+  add
        }
     ans;
  }

  /**
   * Load a sequence of [[VoterRecord]]s from a CSV file
   * @param    filename    The path to a CSV file.
   * @return               The [[VoterRecord]] objects loaded from the file.
   * 
   * This function should make the following assumptions:
   * 1. The first line of the CSV file is a header.  
   * 2. Header fields are 
   *     * "First Name"
   *     * "Last Name"
   *     * "Birthday"
   *     * "Zip Code"
   * 3. Every subsequent line may be split into fields with [[String]]'s split method
   * 4. Columns containing dates are in a format interpretable by [[parseDate]]
   * 
   * Examples of valid CSV files can be found in `src/test/resources/`
   */
  def loadVoterRecords(filename: File): Seq[VoterRecord] = {
    val inputFile = Source.fromFile(filename)
    val lines = inputFile.getLines()
    var ans : Seq[VoterRecord] = Seq()
    lines.drop(1)
    for (line <- lines) {
      val rowData = line.split(",",4)
      val firstName : String  = rowData(0)
      val lastName : String =   rowData(1)
      var birthday: Date = null

    if (rowData(2).nonEmpty) {
       birthday = parseDate(rowData(2))
    }
      var zipcode  : String = null
      if (rowData(3).nonEmpty) {
         zipcode   = rowData(3)
      }
      val add =  VoterRecord(firstName,lastName,birthday,zipcode)
     ans = ans :+  add
    }
    ans;
  }

  /**
   * De-anonymize a collection of "anonymized" [[HealthRecord]] objects using [[VoterRecord]]s
   * @param   voterRecords   A [[Seq]]uence of [[VoterRecord]]s containing names.
   * @param   healthRecords  A [[Seq]]uence of "anonymized" [[HealthRecord]]s.
   * @return                 A [[Map]] of Full Names associated with their [[HealthRecord]]s.
   *
   * For every [[HealthRecord]] that can be **uniquely** linked to a 
   * [[VoterRecord]], this function should return a key-value pair.
   * The key should be the return value of the [[VoterRecord]]'s `fullName` 
   * method.  The value should be the (**unique**) associated 
   * [[HealthRecord]].
   * 
   * If a [[VoterRecord]] can not be associated to any [[HealthRecord]], 
   * or if it can not be **uniquely** associated to just one 
   * [[HealthRecord]], it should not be present in the result set.
   * 
   * This function **must** run in O(voterRecords.size + healthRecords.size)
   */
  def identifyPersons(voterRecords: Seq[VoterRecord], healthRecords: Seq[HealthRecord]): mutable.Map[String, HealthRecord] = {
    val ans: mutable.Map[String, HealthRecord] = mutable.Map()
    val vote_map: mutable.HashMap[(String,Date), List[VoterRecord]] = new mutable.HashMap[(String ,Date),List[VoterRecord]]
    val vote_bir: mutable.HashMap[(Date), List[VoterRecord]] = new mutable.HashMap[(Date),List[VoterRecord]]
    val vote_zip: mutable.HashMap[(String), List[VoterRecord]] = new mutable.HashMap[(String),List[VoterRecord]]
    val birth :  mutable.HashMap[Date, List[VoterRecord]] = new mutable.HashMap[Date, List[VoterRecord]]
 //   val ALL_voter_birth :  mutable.HashMap[Date, List[VoterRecord]] = new mutable.HashMap[Date, List[VoterRecord]]
 //   val ALL_health_birth :  mutable.HashMap[Date, List[HealthRecord]] = new mutable.HashMap[Date, List[HealthRecord]]
    val check_birth :  mutable.HashMap[Date, List[HealthRecord]] = new mutable.HashMap[Date, List[HealthRecord]]
    val dup:  mutable.HashMap[(String,Date), List[HealthRecord]] = new mutable.HashMap[(String,Date), List[HealthRecord]]
    val zip :  mutable.HashMap[String, List[VoterRecord]] = new mutable.HashMap[ String, List[VoterRecord]]
 //   val ALL_voter_zip :  mutable.HashMap[String, List[VoterRecord]] = new mutable.HashMap[ String, List[VoterRecord]]
  //  val ALL_health_zip :  mutable.HashMap[String, List[HealthRecord]] = new mutable.HashMap[ String, List[HealthRecord]]
    val check_zip :  mutable.HashMap[String, List[HealthRecord] ]= new mutable.HashMap[ String,  List[HealthRecord] ]

    for (i <- voterRecords) {
      /** checking if the zipcode as key already exists * */
      if (i.m_ZipCode != null) {
        if (i.m_Birthday != null) {
          val key = (i.m_ZipCode, i.m_Birthday)
        /**  if (!vote_map.contains(key) && birth.contains(i.m_Birthday) /**&& ALL_voter_birth.contains(i.m_Birthday)**/) {
            birth.remove(i.m_Birthday)
          }
          if (!vote_map.contains(key) && zip.contains(i.m_ZipCode) /** && ALL_voter_zip.contains(i.m_ZipCode)**/) {
            zip.remove(i.m_ZipCode)
          }**/
          if (!vote_map.contains(key)) {
            // val value = vote_map((i.m_ZipCode,i.m_Birthday))
            /** if the the Birthdays are also same "they are duplicate " * */
            vote_map(key) = List(i)
            vote_bir(i.m_Birthday) = List(i)
            vote_zip(i.m_ZipCode) = List(i)
          //  ALL_voter_birth(i.m_Birthday) = List(i)
       //     ALL_voter_zip(i.m_ZipCode) = List(i)
          }

          /** checking if the zipcode as key does not exists * */
          else {
            vote_map(key) = i +: vote_map(key)
            vote_bir(i.m_Birthday) = i +:  vote_bir(i.m_Birthday)
            vote_zip(i.m_ZipCode) = i +:   vote_zip(i.m_ZipCode)
          //  ALL_voter_birth(i.m_Birthday) = i +: ALL_voter_birth(i.m_Birthday)
          //  ALL_voter_zip(i.m_ZipCode) = i +: ALL_voter_zip(i.m_ZipCode)
          }

        }
      }
    }
      for (i <- voterRecords) {
        if (i.m_ZipCode != null) {
          if (i.m_Birthday == null) {
            val key = i.m_ZipCode
            if (!zip.contains(key) /** && !ALL_voter_zip.contains(key) **/) {
              zip(key) = List(i)
             // ALL_voter_zip(i.m_ZipCode) = List(i)
            }
            else {
              zip(key) = i +: zip(key)

            }
          }
        }
        else if (i.m_ZipCode == null) {
          if (i.m_Birthday != null) {
            val key = i.m_Birthday
            if (key != null) {
              if (!birth.contains(key) /** && !ALL_voter_birth.contains(key) **/) {
                birth(key) = List(i)
           //     ALL_voter_birth(i.m_Birthday) = List(i)
              }
              else {
                birth(key) = i +: birth(key)
              }
            }
          }
        }
      }

   for (j <- healthRecords) {
     if (j.m_ZipCode != null) {
       if (j.m_Birthday != null) {
         val key = (j.m_ZipCode, j.m_Birthday)
         val Bkey = j.m_Birthday
         val Zkey = j.m_ZipCode

         /** if (!dup.contains(key) && check_birth.contains(j.m_Birthday) /** && ALL_health_birth.contains(j.m_Birthday)**/) {
          * ans.remove(birth(j.m_Birthday).head.fullName)
          * check_birth.remove(j.m_Birthday)
          * }
          * if (!dup.contains(key) && check_zip.contains(j.m_ZipCode) /** && ALL_health_zip.contains(j.m_ZipCode)**/) {
          * ans.remove(zip(j.m_ZipCode).head.fullName)
          * // check_zip.remove(j.m_ZipCode)
          * } * */

         if (vote_map.contains(key) ){
               if ( !dup.contains(key)) {
                 val value = vote_map(key)
                 if (value.size == 1) {
                   val name = value.head.fullName
                   ans(name) = j
                 }
                 dup(key) = List(j)
                 //  ALL_health_birth(j.m_Birthday) = List(j)
                 //  ALL_health_zip(j.m_ZipCode) = List(j)
                 vote_map.remove(key)
                 vote_zip.remove(j.m_ZipCode)
                 vote_bir.remove(j.m_Birthday)
               }

               else {
                 if (ans.contains(vote_map(key).head.fullName)) {
                   ans.remove(vote_map(key).head.fullName)
                 }
               }
         }
        else if (birth.contains(Bkey) ){
           if ( !check_birth.contains(Bkey)) {
             val value = birth(Bkey)
             if (value.size == 1) {
               val name = value.head.fullName
               ans(name) = j
             }
             check_birth(Bkey) = List(j)
             birth.remove(Bkey)
           }
           else{
             if (birth.contains(j.m_Birthday) && ans.contains(birth(j.m_Birthday).head.fullName)) {
               ans.remove(birth(j.m_Birthday).head.fullName)
             }
           }

         }
         else if (zip.contains(Zkey) ){
           if ( !check_zip.contains(Zkey)) {
             val value = zip(Zkey)
             if (value.size == 1) {
               val name = value.head.fullName
               ans(name) = j
             }
             check_zip(Zkey) = List(j)
             zip.remove(Zkey)
           }
           else {
             if ( zip.contains(j.m_ZipCode) && ans.contains(zip(j.m_ZipCode).head.fullName)) {
               ans.remove(zip(j.m_ZipCode).head.fullName)
             }
           }
         }
       }
     }
   }
     for (j <- healthRecords) {
       if (j.m_ZipCode != null) {
          if (j.m_Birthday == null) {
               val key = (j.m_ZipCode, null)
            val key2 = (j.m_ZipCode)
            if (vote_zip.contains(key2) ){
                 if ( !dup.contains(key)) {
                   val value = vote_zip(key2)
                   if (value.size == 1) {
                     val name = value.head.fullName
                     ans(name) = j
                   }
                   dup(key) = List(j)
                   //  ALL_health_birth(j.m_Birthday) = List(j)
                   //  ALL_health_zip(j.m_ZipCode) = List(j)
                   vote_map.remove(key)
                   vote_zip.remove(key2)
                 }
                 else {
                   if (ans.contains(vote_map(key).head.fullName)) {
                     ans.remove(vote_map(key).head.fullName)
                   }
                 }
            }
            else if (zip.contains(j.m_ZipCode)) {
                 if ( !check_zip.contains(j.m_ZipCode) && !check_zip.contains(j.m_ZipCode)/** && !ALL_health_zip.contains(j.m_ZipCode)* */) {
                       val value = zip(j.m_ZipCode)
                       if (value.size == 1) {
                         val name = value.head.fullName
                         ans(name) = j
                         // ALL_health_zip(j.m_ZipCode) = List(j)
                       }
                       check_zip(j.m_ZipCode) = List(j)
                 }
                 else {
                       if ( ans.contains(zip(j.m_ZipCode).head.fullName)) {
                         ans.remove(zip(j.m_ZipCode).head.fullName)
                       }
                 }
            }
          }
     }
     else if (j.m_ZipCode == null) {
       if (j.m_Birthday != null) {
         val key = (j.m_ZipCode,j.m_Birthday)
         val key2 = (j.m_Birthday)
         if (vote_bir.contains(key2) ){
           if ( !dup.contains(key)) {
             val value = vote_bir(key2)
             if (value.size == 1) {
               val name = value.head.fullName
               ans(name) = j
             }
             dup(key) = List(j)
             //  ALL_health_birth(j.m_Birthday) = List(j)
             //  ALL_health_zip(j.m_ZipCode) = List(j)
             vote_map.remove(key)
             vote_bir.remove(key2)
           }

           else {
             if (ans.contains(vote_map(key).head.fullName)) {
               ans.remove(vote_map(key).head.fullName)
             }
           }
         }
         else if (birth.contains(j.m_Birthday)) {
           if ( !check_birth.contains(j.m_Birthday)/** &&  !ALL_health_birth.contains(j.m_Birthday)* */) {
             val value = birth(j.m_Birthday)
             if (value.size == 1) {
               val name = value.head.fullName
               ans(name) = j
               // ALL_health_birth(j.m_Birthday) = List(j)
             }
             check_birth(j.m_Birthday) = List(j)
           }
           else {
             if ( ans.contains(birth(j.m_Birthday).head.fullName)) {
               ans.remove(birth(j.m_Birthday).head.fullName)
             }
           }
         }
       }
     }
   }
    ans
 }

  /**
   * Compute a histogram over one of the attributes of HealthRecord
   * @param    records      A [[Sequence]] of [[HealthRecord]]s
   * @param    attribute    Either [[HealthRecordBirthday]] 
   *                        or [[HealthRecordZipCode]]
   * @return                A key-value pair of each stringified attribute
   *                        value and the percentage of the records that
   *                        have this value.  The percentage should be
   *                        a value in the range (0, 1]
   * 
   * If attribute == HealthRecordBirthday, use [[HealthRecord]]'s 
   * `m_Birthday` field.
   * 
   * If attribute == HealthRecordZipCode, use [[HealthRecord]]'s 
   * `m_ZipCode` field.
   * 
   * This function **must** run in O(healthRecords.size)
   */
  def computeHealthRecordDist(records: Seq[HealthRecord], attribute: HealthRecordAttribute
  ): mutable.Map[String, Double] = {
    val len : Int = records.length
    val ans: mutable.Map[String, Double] = new mutable.HashMap[String, Double]
    val ans2: mutable.Map[String, Double] = new mutable.HashMap[String, Double]
    for (j <- records) {
      var compare = ""
      if (attribute == HealthRecordBirthday) {
              compare = j.m_Birthday.toString
              if (ans.contains(compare)){
                  var current = ans(compare)
                  current = current + 1
                  ans(compare) = current
              }
              else {
                  ans += (compare -> 1)
              }
              val raw_num = ans(compare)
              val percentage: Double = (raw_num / len)
              ans2(compare) = percentage
      }
      else if (attribute == HealthRecordZipCode) {
              compare = j.m_ZipCode
              if (ans.contains(compare)) {
                  var current = ans(compare)
                  current = current + 1
                  ans(compare) = current
              }
              else {
                  ans += (compare -> 1)
              }
              val raw_num = ans(compare)
              val percentage: Double = (raw_num / len)
              ans2(compare) = percentage
      }
    }
    ans2
  }

}
