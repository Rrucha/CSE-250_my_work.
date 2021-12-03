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
    val vote_map: mutable.HashMap[(String,Date), VoterRecord] = new mutable.HashMap[(String ,Date),VoterRecord]
    val duplicate_vote_map: mutable.HashMap[(String,Date), VoterRecord] = new mutable.HashMap[(String ,Date),VoterRecord]
    val birth :  mutable.HashMap[Date, VoterRecord] = new mutable.HashMap[Date, VoterRecord]
    val dup_birth :  mutable.HashMap[Date, VoterRecord] = new mutable.HashMap[Date, VoterRecord]
    val dup_check_birth :  mutable.HashMap[Date, HealthRecord] = new mutable.HashMap[Date, HealthRecord]
    val zip :  mutable.HashMap[String, VoterRecord] = new mutable.HashMap[ String, VoterRecord]
    val dup_zip :  mutable.HashMap[String, VoterRecord] = new mutable.HashMap[ String, VoterRecord]
    val dup_check_zip :  mutable.HashMap[String, HealthRecord] = new mutable.HashMap[ String, HealthRecord]
    val dup_check: mutable.HashMap[(String,Date), HealthRecord] = new mutable.HashMap[(String ,Date),HealthRecord]
    for (i <- voterRecords){
      /**checking if the zipcode as key already exists **/
      if (i.m_ZipCode == null) {
        if (i.m_Birthday != null) {
         if (birth.contains(i.m_Birthday) && !dup_birth.contains(i.m_Birthday)){
           dup_birth(i.m_Birthday) = i
           birth.-(i.m_Birthday)
         }
         else if (!birth.contains(i.m_Birthday)){
           if (!dup_birth.contains(i.m_Birthday))
           birth(i.m_Birthday) = i
         }
        }
      }
      else if (i.m_ZipCode != null) {
          if (i.m_Birthday != null) {
            if (vote_map.contains(i.m_ZipCode,i.m_Birthday)) {
                 // val value = vote_map((i.m_ZipCode,i.m_Birthday))
                  /** if the the Birthdays are also same "they are duplicate " * */
                   duplicate_vote_map((i.m_ZipCode,i.m_Birthday)) = i
                   vote_map.-((i.m_ZipCode,i.m_Birthday))
            }
                /** checking if the zipcode as key does not exists * */
            else {
              if (!duplicate_vote_map.contains((i.m_ZipCode,i.m_Birthday))) {
                vote_map((i.m_ZipCode, i.m_Birthday)) = i
              }
            }
           }
          if (i.m_Birthday == null) {
            if (zip.contains(i.m_ZipCode)){
              dup_zip(i.m_ZipCode) = i
              zip.-(i.m_ZipCode)
            }
            else{
              if (!dup_zip.contains(i.m_ZipCode))
                zip(i.m_ZipCode) = i
            }

          }
      }
   }
   for (j <- healthRecords) {
     if (j.m_ZipCode == null) {
       if (j.m_Birthday != null) {
         if (!dup_check_birth.contains(j.m_Birthday)) {
           if (birth.contains(j.m_Birthday) && !dup_birth.contains(j.m_Birthday)) {
             val value = birth(j.m_Birthday)
             val name = value.fullName
             ans(name) = j

           }
           dup_check_birth(j.m_Birthday) = j
         }
           else{
             ans.remove(birth(j.m_Birthday).fullName)
           }

       }
     }
     if (j.m_ZipCode != null) {
       if (j.m_Birthday != null) {
         val key = (j.m_ZipCode, j.m_Birthday)
         if (!dup_check.contains(key)) {
           if (!duplicate_vote_map.contains(key) && vote_map.contains(key) ) {
             val value = vote_map(key)
             val name = value.fullName
             ans(name) = j
           }
           dup_check(key) = j
         }
         else {
           ans.remove(vote_map(key).fullName)
         }
       }
       else if (j.m_Birthday == null) {
            if (!dup_check_zip.contains(j.m_ZipCode)) {
              if (zip.contains(j.m_ZipCode) && !dup_zip.contains(j.m_ZipCode)) {
               val value = zip(j.m_ZipCode)
               val name = value.fullName
               ans(name) = j
              }
              dup_check_zip(j.m_ZipCode) = j
            }
            else{
             if (ans.contains(zip(j.m_ZipCode).fullName)) {
                ans.remove(zip(j.m_ZipCode).fullName)
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
