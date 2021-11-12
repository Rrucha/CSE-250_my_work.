/**
 * cse250.pa2.AppendOnlyLSMIndex
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
package cse250.pa2

import scala.collection.Searching.{Found, InsertionPoint, SearchResult, search}
import scala.reflect.ClassTag
import scala.util.Sorting
import scala.collection.mutable

/**
 * An implementation of the Log Structured Merge Tree (LSM Index)
 * supporting only appends (no deletions).
 */
class LSMIndex[K:Ordering, V <: AnyRef](_bufferSize: Int)(implicit ktag: ClassTag[K], vtag: ClassTag[V])
{
  implicit val _ordering = new Ordering[(K, V)]{
    def compare(a: (K, V), b: (K, V)): Int =
      Ordering[K].compare(a._1, b._1)
  }


  /**
   * The input buffer.  Elements reside here until _buffer is
   * full, at which point they are sorted and placed into 
   * _levels.
   */
  val _buffer = new Array[(K,V)](_bufferSize)

  /**
   * The number of elements of _buffer used.
   */
  var _bufferElementsUsed = 0

  /**
   * A sequence of sorted arrays of progressively growing sizes
   * In general, _levels(i).size == 2**i * _bufferSize
   */
  val _levels = mutable.ArrayBuffer[Option[IndexedSeq[(K,V)]]]()


  /**
   * Insert a key, value pair into the LSM Index.
   * @param    key      The key of the record to be inserted
   * @param    value    The value of the record to be inserted
   * 
   * This function should run in amortized O(log(n)) time
   */
  def insert(key:K, value:V): Unit =
  {
    // Append the key/value pair to the end of the 
    // buffer.
    _buffer(_bufferElementsUsed) = (key, value)
    _bufferElementsUsed += 1

    // If the buffer is full, sort and promote the
    // buffer to level 0
    if(_bufferElementsUsed >= _bufferSize)
    {
      Sorting.quickSort(_buffer)
      promote(0, _buffer.toIndexedSeq)
      _bufferElementsUsed = 0
    }
  }

  /**
   * Install a new sequence at the specified level
   * @param  level          The level to install the sequence at
   * @param  layerContents  The sequence of elements to install at the layer
   */
  def promote(level: Int, layerContents: IndexedSeq[(K, V)]): Unit = {
   if (_levels.isEmpty){
     _levels += Option(layerContents)
   }
   else if (_levels.size <= level){
      _levels += Option(layerContents)
   }
    else if (_levels(level).nonEmpty){
     val what = MergedIterator.merge[(K,V)](_levels(level).get,layerContents)
      _levels(level) = None
      promote(level +1, what)
    }

    else{
       _levels(level) = Option(layerContents)

    }
  }

  /**
   * Determine if the provided key is present in the LSM index
   * @param  key       The key to look for in the index
   * @return           True if the key is present
   * 
   * This function should run in O(log^2(n)) time
   */
  def contains(key: K): Boolean = {
    var k = 0
    var check = 0;
    while (k <= _bufferElementsUsed) {
      val some = _buffer(k)._1
      if (_buffer(k)._1 == key) {
          return true
         }
      k = k + 1
    }
    var i = 0
    while(i != _levels.size) {
       if(_levels(i) != None){
      val x: SearchResult = _levels(i).get.search(key, null.asInstanceOf[V])
      x match {
        case Found(idx) => return true;
        case InsertionPoint(idx) => check = -2;
      }
      i = i + 1
    }
      else{
        i = i + 1
      }
    }
    if (check == -2){
       false
    }
 else{
       true
    }
  }

  /**
   * Retrieve the value associated with the provided key from the
   * LSM index
   * @param  key       The key to look for in the index
   * @return           A sequence of values associated with the
   *                   specified key
   * 
   * This function should run in O(log^2(n)) time
   */
  def apply(key: K): Seq[V] = {
    var k = 0
    var bufferpountcheck = -2;
    var check = 0;
    var ans: Seq[V] = Seq()
    while (k < _bufferElementsUsed) {
      if (_buffer(k)._1 == key) {
        val foundValue = _buffer(k)._2
        ans = ans :+ foundValue
        bufferpountcheck = 1;
      }
      k = k + 1
    }
    var i = 0
    while(i != _levels.size) {
      val copy = _levels(i)
     if (copy != None){
      val x: SearchResult = copy.get.search(key, null.asInstanceOf[V])
      x match {
        case Found(idx) => {
          check = 0
          var bruh = 0
          var acc1 = idx
          while (acc1 >= 0 && acc1 != copy.get.size && copy.get(acc1)._1 == key && copy.get.size - 1 != idx) {
            ans = ans :+ copy.get(acc1)._2
            acc1 = acc1 + 1
            bruh = -2
          }
          var acc = idx
          if (bruh == -2) {
            acc = acc - 1
            bruh = 0
          }
          while (acc >= 0 && acc != copy.get.size && copy.get(acc)._1 == key && idx != 0) {
            ans = ans :+ copy.get(acc)._2
            acc = acc - 1
          }
        }
        case InsertionPoint(idx) => {
          if (bufferpountcheck == 1) {
            check = 0;
          }
          else{
            check = -2
          }
        }
      }
      i = i + 1
    }
      else{
        i = i + 1
      }

    }
    if (check == -2) {
       return Seq.empty
    }
    else{
       return ans
    }
  }

  /**
   * Generate a string representation of this LSM index
   */
  override def toString: String = 
    s"Buffer (${_bufferElementsUsed} elements): " + 
  _buffer.take(_bufferElementsUsed).map { _._1 }.mkString(", ") + "\n" +
    _levels.zipWithIndex
           .map { case (level, i) => 
              s"Level $i: " + (level match {
                case None => "[Unused]"
                case Some(contents) => contents.map { _._1 }.mkString(", ")
              })
            }
           .mkString("\n")

}