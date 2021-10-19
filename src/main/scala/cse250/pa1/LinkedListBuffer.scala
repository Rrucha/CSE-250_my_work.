/**
 * cse250.pa1.LinkedListBuffer
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
package cse250.pa1

import scala.+:
import scala.util.control.Breaks
import scala.util.control.Breaks.break

class LinkedListBuffer[A](capacity: Int)
  extends scala.collection.mutable.Seq[A]
{
  val _buffer = Array.fill[LinkedListNode](capacity) { new LinkedListNode(None) }
  var _numStored = 0
  var _head = -1
  var _tail = -1

  /**
   * Append an entry into the sequence, displacing the oldest entry if needed.
   * @param     entry       The entry to insert
   * @return                The entry that was displaced, or None
   * 
   * After this function is called, `entry` should be the final element of the 
   * sequence.  If adding `entry` would cause the size of the sequence to 
   * exceed its `capacity`, the first entry in the sequence should be removed.
   * 
   * Data may only be stored in `_buffer`.  Your solution may not use any other
   * collection types.
   * 
   * This function must run in O(n) time, where n = [[length]].
   * 
   * 2 bonus points will be awarded if this function runs in Θ(1) time 
   * 
   * (assume that all times are non-amortized unless otherwise specified).
   */
  def append(entry: A): Option[A] = {

    if (_numStored < capacity){
      var status : Option[A] = _buffer(_numStored)._value
      val arr = new LinkedListNode(Some(entry))
      if (_numStored == 0){
        status =  _buffer(_numStored)._value
        _buffer(0)= arr
        arr._next = -1
        arr._prev = -1

        _head = 0
        _tail =0
        _numStored = _numStored + 1
      }
      else if (_numStored == capacity-1){
        status = _buffer(_numStored)._value
        arr._next = -1
        arr._prev = _tail

        _buffer(_numStored) =  arr
        _buffer(_tail)._next = _buffer.indexOf(arr)

        _tail = _buffer.indexOf(arr)
        _numStored = _numStored + 1
      }
      else{
        status =  _buffer(_numStored)._value
        arr._next = -1
        arr._prev = _tail
        _buffer(_numStored)= arr
        _buffer(_tail)._next = _buffer.indexOf(arr)
        _tail = _buffer.indexOf(arr)
        _numStored = _numStored + 1
      }
status
    }
    else {
      val arr = new LinkedListNode(Some(entry))
      val before = _buffer(_head)
      val index : Int=  _head
      _head = _buffer(_head)._next
      _buffer(_head)._prev = -1
      arr._prev = _tail
      arr._next = -1
      _buffer(index) = arr
      _buffer(_tail)._next = _buffer.indexOf(arr)
      _tail = _buffer.indexOf(arr)

      before._value
    }
  }


  /**
   * Remove all instances of an element from the sequence.
   * @param     entry       The entry to remove.
   * @return                True if at least one instance of the entry was 
   *                        removed.
   * 
   * After this function is called, the sequence should contain no elements
   * that are equal (according to `==`) to `entry`.
   * 
   * This function must run in O(n) time, where n = [[length]] 
   */
  def remove(entry: A): Boolean = {
    var check = 9
     for (i <- _buffer) {
       if (i.get == entry){
         var PREVIOUS: LinkedListNode = null
         var NEC: LinkedListNode = null
         if (i._prev != -1) {
            PREVIOUS = _buffer(i._prev)
         }
         if (i == _buffer(_head)){
           if (i._next != -1) {
             _head = i._next

           }
           else{
             _head = -1
           }
         }
         if (i == _buffer(_tail)){
           if (i._prev != -1) {
             _tail = i._prev
             _buffer(i._prev)._next = -1
           }
           else{
             _tail= -1
           }
         }
         if (i._next != -1) {
           NEC = _buffer(i._next)
         }
         val ing  = _buffer.indexOf(i)

         _buffer.drop(ing)

         if (i._next != -1 && PREVIOUS != null) {
           _buffer(i._next)._prev = _buffer.indexOf(_buffer(i._prev))
         }

         if (i._prev != -1 && NEC != null) {
           _buffer(i._prev)._next = _buffer.indexOf(_buffer(i._next))
         }
         else if (i._prev != -1 && NEC == null){
           _buffer(i._prev)._next = -1
         }
         check = -1
         _numStored = _numStored - 1
       }
     }
   if (check == -1){
     true
   }
   else{
      false
    }
  }

  /**
   * Return the current length of the sequence
   * @return                The number of elements in the sequence
   * 
   * Note that this is NOT exactly the same as the number of calls to append 
   * minus the number of calls to remove, since append will not increase the
   * size of the sequence beyond the sequence's `capacity`.
   * 
   * This function must run in Θ(1) time.
   */
  override def length: Int = {
    var num = 0
    var i = _head
    if (i != -1) {
      while (i != -1) {
        num = num + 1
        if (_buffer(i)._next != -1) {
          i = _buffer(i)._next
        }
        else{
          i = -1
        }
      }

    }
    num
  }

  /**
   * Retrieve the `idx`th element to be inserted into the sequence (i.e., the
   * `idx`th element in insertion order)
   * @param    idx      The position of the element to retrieve.
   * @return            The element at position `idx`
   * 
   * The order of the sequence is the order in which elements were inserted.
   * 
   * This function must run in O(n) time, where n = `idx`
   */
  override def apply(idx: Int): A ={
    var oof: LinkedListNode = _buffer.head
    var oo = _head

    var num = 0
    var _curr = _head
    var bruh: LinkedListNode = _buffer(oo)
    while (oo != -1){
      if (num <= idx) {
        num = num + 1
        bruh = _buffer(oo)
      }

      if (_buffer(oo)._next != -1){
        oo = _buffer(oo)._next
      }
      else{
       oo =  -1
      }

    }

    bruh.get
  }

  /**
   * Count the number of times `entry` occurs in the sequence.
   * @param    entry     The entry to count in the sequence
   * @return             The number of times entry (according to ==) occurs
   * 
   * Count the number of times the specified `entry` occurs in the sequence.
   * 
   * 
   * This function must run in O(n) time, where n = [[length]] 
   */
  def countEntry(entry: A): Int = {
    var num = 0
    for (i <- _buffer) {
      if (i.get == entry){
        num = num +1
      }
    }
    num
  }

  /**
   * Update the value at position `idx` to `elem`
   * @param    idx       The index to update
   * @param    elem      The element to update the sequence to
   * 
   * Modify the sequence, replacing the element previously at position `idx` 
   * with `elem`.  This does not otherwise change the order of the list.
   * 
   * This function must run in O(n) time, where n = `idx`
   */
  def update(idx: Int, elem: A): Unit = {
    if (idx <= _buffer.length) {
      var oof: LinkedListNode = _buffer.head
      var oo = _head
      var num = 0
      var _curr = _head
      var bruh: LinkedListNode = _buffer(oo)
      while (oo != -1){
        if (num <= idx) {
          num = num + 1
          bruh = _buffer(oo)
        }

        if (_buffer(oo)._next != -1){
          oo = _buffer(oo)._next
        }
        else{
          oo =  -1
        }

      }
      bruh._value = Option(elem)
    }
    else{
      throw new  IndexOutOfBoundsException
    }
  }

  /**
   * Return an iterator over the elements of this sequence
   * @return                An iterator over the elements of this sequence
   * 
   * Iteration should proceed in order of insertion.  The first element to be
   * [[append]]ed should be the first element the iterator visits.  The most
   * recent element to be [[append]]ed should be the last element the iterator 
   * visits
   * 
   * This function must run in Θ(1) time.
   */
  override def iterator: LinkedListIterator =
    new LinkedListIterator(){

    }

  /**
   * Render a graphical representation of the list
   */
  override def toString(): String = 
    iterator.map { "[" + _ + "]" }.mkString(" ↔ ")


  /**
   * One node of a linked list.
   */
  class LinkedListNode(var _value: Option[A])
  {
    /**
     * A reference (pointer) to the position in `_buffer` where the preceding
     * element of the sequence is located, or -1 if this is the first element
     * of the sequence.
     */
    var _prev: Int = -1

    /**
     * A reference (pointer) to the position in `_buffer` where the following
     * element of the sequence is located, or -1 if this is the last element
     * of the sequence.
     */
    var _next: Int = -1

    /**
     * Return true if the linked list node is in-use
     */
    def isSet = _value.isDefined

    /**
     * Assign a value to this node
     */
    def set(value: A) = { _value = Some(value) }

    /**
     * Clear the value in this node
     */
    def clear = { _value = None }

    /**
     * Get the current node value (if set)
     */
    def get = { _value.get }
  }

  /**
   * An iterator over the elements of this linked list.
   */
  class LinkedListIterator extends Iterator[A]
  {
    var _curr = _head

    /**
     * Return true if there are additional elements in this iterator, or 
     * false if the iterator has no further elements.
     * @return        True if next() will return another element.
     * 
     */
    override def hasNext: Boolean = { _curr > -1 }

    /**
     * Return the next element of the iterator, and advance the iterator to 
     * the following position.
     * @return        The sequentially next element from the sequence
     * 
     * This method may throw a [[NoSuchElementException]] if it is called
     * hasNext() returns false.
     */
    override def next(): A = { 
      if(_curr == -1){ throw new NoSuchElementException() }
      
      val currentElement = _buffer(_curr)
      _curr = currentElement._next
      return currentElement._value.get
    }

    /**
     * Remove the last element returned by [[next]] from the underlying list.
     * 
     * This method must throw a [[NoSuchElementException]] if it is called
     * before [[next]] is called for the first time on this iterator.
     */
    def remove(): Unit ={
      var currentElemen: LinkedListNode = null
      var currentElement: LinkedListNode = null
      if(_curr != -1) {
        currentElement = _buffer(_curr)
      }
      else{
          currentElement = _buffer(_tail)
      }

      var PREVIOUS: LinkedListNode = null
      var NEC: LinkedListNode = null
             val  _cur = currentElement._prev
            if (_curr != -1) {
               currentElemen = _buffer(_cur)
            }
      else{
             currentElemen = currentElement
            }
      if (currentElemen._prev != -1) {
        PREVIOUS = _buffer(currentElemen._prev)
      }
      if (currentElemen == _buffer(_head)){
        if (currentElemen._next != -1) {
          _head = currentElemen._next

        }
        else{
          _head = -1
        }
      }
      if (currentElemen == _buffer(_tail)){
        if (currentElemen._prev != -1) {
          _tail = currentElemen._prev
          _buffer(currentElemen._prev)._next = -1
        }
        else{
          _tail= -1
        }
      }
      if (currentElemen._next != -1) {
        NEC = _buffer(currentElemen._next)
      }


      _buffer.drop(_cur)

      if (currentElemen._next != -1 && PREVIOUS != null) {
        _buffer(currentElemen._next)._prev = _buffer.indexOf(_buffer(currentElemen._prev))
      }

      if (currentElemen._prev != -1 && NEC != null) {
        _buffer(currentElemen._prev)._next = _buffer.indexOf(_buffer(currentElemen._next))
      }
      else if (currentElemen._prev != -1 && NEC == null){
        _buffer(currentElemen._prev)._next = -1
      }

      _numStored = _numStored - 1


    }
  }

}