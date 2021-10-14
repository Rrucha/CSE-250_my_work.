/**
 * cse250.pa1.LinkedListBufferTests
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

import cse250.objects.SolarInstallation
import org.scalatest.flatspec.AnyFlatSpec

class LinkedListBufferTests extends AnyFlatSpec {
  
  def createLinkedListBuffer(capacity: Int) = new LinkedListBuffer[SolarInstallation](capacity)

  behavior of "LinkedListBuffer"
  it should "keep a history of elements equal to its capacity" in {

    val buffer = createLinkedListBuffer(3);
    val e1, e2, e3, e4 = new SolarInstallation()

    // Iterator should start empty
    {
      val iterator = buffer.iterator
      assert(!iterator.hasNext)
    }


    buffer.append(e1)

    // Iterator should return just the one element after one append
    {
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert(e1 == iterator.next())
      assert(!iterator.hasNext)
    }

    buffer.append(e2)
    buffer.append(e3)

    // Iterator should return all three elements after 3 appends
    {
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert(e1 == iterator.next())
      assert(iterator.hasNext)
      assert(e2 == iterator.next())
      assert(iterator.hasNext)
      assert(e3 == iterator.next())
      assert(!iterator.hasNext)
    }

    buffer.append(e4)

    // Appending e4 to a full iterator should displace e1
    {
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert(e2 == iterator.next())
      assert(iterator.hasNext)
      assert(e3 == iterator.next())
      assert(iterator.hasNext)
      assert(e4 == iterator.next())
      assert(!iterator.hasNext)
    }
  }

    /*** start of APPEND TESTING**/
               /*** start of APPEND TESTING CASE 1**/
        behavior of "LinkedListBuffer"
          it should "Append properly CASE 1: normal situation" in {
            val buffer = createLinkedListBuffer(3);
            val e1, e2, e3, e4 ,e5 = new SolarInstallation()

            // Iterator should start empty
            {
              val iterator = buffer.iterator
              assert(!iterator.hasNext)
            }

            buffer.append(e1)

            {
              val iterator = buffer.iterator
              assert(iterator.next() == e1)
              assert(buffer.length == 1)
              assert(buffer.apply(buffer.length-1) == e1)
            }




            buffer.append(e2)

            {
              val iterator = buffer.iterator
              assert( iterator.next() == e1)
              assert( iterator.next() == e2)
              assert(buffer.length == 2)
              assert(buffer.apply(buffer.length-1) == e2)
            }

            buffer.append(e3)

            {
              val iterator = buffer.iterator
              assert( iterator.next() == e1)
              assert( iterator.next() == e2)
              assert( iterator.next() == e3)
              assert(buffer.length == 3)
              assert(buffer.apply(buffer.length-1) == e3)
            }

        }
    /*** END of APPEND TESTING CASE 1**/
    /*** start of APPEND TESTING CASE 2**/

    behavior of "LinkedListBuffer"
    it should "Append properly CASE 2 MIX ORDER" in {
      val buffer = createLinkedListBuffer(3);
      val e1, e2, e3, e4 ,e5 = new SolarInstallation()

      // Iterator should start empty
      {
        val iterator = buffer.iterator
        assert(!iterator.hasNext)
      }

      buffer.append(e2)

      {
        val iterator = buffer.iterator
        assert( iterator.next() == e2)
        assert(buffer.length == 1)
        assert(buffer.apply(buffer.length -1)== e2)
      }

      buffer.append(e1)

      {
        val iterator = buffer.iterator
        assert( iterator.next() == e2)
        assert( iterator.next() == e1)
        assert(buffer.length == 2)
        assert(buffer.apply(buffer.length-1) == e1)
      }

      buffer.append(e3)

      {
        val iterator = buffer.iterator
        assert( iterator.next() == e2)
        assert( iterator.next() == e1)
        assert( iterator.next() == e3)
        assert(buffer.length == 3)
        assert(buffer.apply(buffer.length-1) == e3)
      }

    }
    /***END  of APPEND TESTING CASE 2**/
    /*** start of APPEND TESTING CASE 3**/

    behavior of "LinkedListBuffer"
    it should "Append properly CASE 3 CAPACITY FULL" in {
      val buffer = createLinkedListBuffer(4);
      val e1, e2, e3, e4 ,e5 = new SolarInstallation()

      // Iterator should start empty
      {
        val iterator = buffer.iterator
        assert(!iterator.hasNext)
      }

     assert( buffer.append(e2) == None)

      {
        val iterator = buffer.iterator
        assert( iterator.next() == e2)
        assert(buffer.length == 1)

        assert(buffer.apply(buffer.length-1) == e2)
      }




      assert(buffer.append(e1)==None)

      {
        val iterator = buffer.iterator
        assert( iterator.next() == e2)
        assert( iterator.next() == e1)
        assert(buffer.length == 2)
        assert(buffer.apply(buffer.length-1) == e1)
      }

     assert( buffer.append(e3) == None)

      {
        val iterator = buffer.iterator
        assert( iterator.next() == e2)
        assert( iterator.next() == e1)
        assert( iterator.next() == e3)
        assert(buffer.length == 3)
        assert(buffer.apply(buffer.length-1) == e3)
      }

    assert( buffer.append(e4) == None)

      {

        val iterator = buffer.iterator
        assert( iterator.next() == e2)
        assert( iterator.next() == e1)
        assert( iterator.next() == e3)
        assert( iterator.next() == e4)
        assert(buffer.length == 4)
        assert(buffer.apply(buffer.length-1) == e4)
      }

     assert(buffer.append(e3) == Some(e2))

      {
        val iterator = buffer.iterator
        assert( iterator.next() == e1)
        assert( iterator.next() == e3)
        assert( iterator.next() == e4)
        assert( iterator.next() == e3)
        assert(buffer.length == 4)
        assert(buffer.apply(buffer.length-1) == e3)
      }
      /*** END of APPEND TESTING CASE 3**/
    }

          /*** END of APPEND TESTING**/
    /*** START of REMOVE TESTING**/
         /*** start of REMOVE TESTING CASE 1**/
    behavior of "LinkedListBuffer"
    it should "Remove properly Case 1 EVERYTHING PERFECT" in {
      val buffer = createLinkedListBuffer(4);
      val r1, r2, r3, r4 ,r5 = new SolarInstallation()

      buffer.append(r1)

      buffer.append(r2)

      buffer.append(r4)

      buffer.append(r5)

      {
        val Bool = buffer.remove(r3)
        assert(!Bool)
      }

      {
        val Bool: Boolean = buffer.remove(r4)
        assert(Bool)
        assert(buffer.length == 3)

      }
    }
    /*** END of REMOVE TESTING CASE 1**/
    /*** start of REMOVE TESTING CASE 2**/
    behavior of "LinkedListBuffer"
    it should "Remove properly Case 2 REMOVE HEAD" in {
      val buffer = createLinkedListBuffer(4);
      val r21, r22, r23, r24 ,r25 = new SolarInstallation()

      buffer.append(r21)

      buffer.append(r22)

      buffer.append(r24)

      buffer.append(r25)

      {
        val Bool = buffer.remove(r23)
        assert(!Bool)
      }

      {
        val Bool: Boolean = buffer.remove(r21)
        assert(Bool)
        assert(buffer.length == 3)

        assert(buffer.apply(0) == r22 )
      }
    }
    /*** END of REMOVE TESTING CASE 2**/
    /*** start of REMOVE TESTING CASE 3**/

    behavior of "LinkedListBuffer"
    it should "Remove properly Case 3 REMOVE TAIL" in {
      val buffer = createLinkedListBuffer(4);
      val r31, r32, r33, r34 ,r35 = new SolarInstallation()

      buffer.append(r31)

      buffer.append(r32)

      buffer.append(r34)
      buffer.append(r35)


      {
        val Bool = buffer.remove(r33)
        assert(!Bool)
      }

      {
        val Bool: Boolean = buffer.remove(r35)
        assert(Bool)
        assert(buffer.length == 3)
        assert(buffer.apply(buffer.length-1) == r34)
      }
    }
         /*** END of REMOVE TESTING CASE 3**/

    /*** start of REMOVE TESTING CASE 4**/

    behavior of "LinkedListBuffer"
    it should "Remove properly Case 4 REMOVE WHEN ONLY ONE NODE IS THERE" in {
      val buffer = createLinkedListBuffer(4);
      val r41, r42, r43, r44 ,r45 = new SolarInstallation()

      buffer.append(r41)

      {
        val Bool : Boolean= buffer.remove(r43)
        assert(!Bool)
      }

      {
        val Bool: Boolean = buffer.remove(r45)
        assert(Bool)
        assert(buffer.length == 0)
        assert(buffer.head != r41)
        assert(buffer.apply(buffer.length-1) != r41)
      }
    }

  /*** END of REMOVE TESTING CASE 4**/
           /*** END of REMOVE TESTING**/
  /*** START of COUNT ENTRY TESTING**/
        /*** START of COUNT ENTRY TESTING CASE 1  **/

    behavior of "LinkedListBuffer"
    it should "Count Entry properly CASE1 " in {
      val buffer = createLinkedListBuffer(4);
      val c1, c2, c3, c4 ,c5 = new SolarInstallation()

      buffer.append(c1)

      buffer.append(c2)

      buffer.append(c4)

      buffer.append(c1)

      {
        val count: Int = buffer.countEntry(c3)
        assert(count == 0)
      }

      {
        val count: Int = buffer.countEntry(c1)
        assert(count == 2)
        assert(buffer.length == 3)
        assert(buffer.apply(0) == c1)
        assert(buffer.apply(buffer.length -1) == c1)

      }
    }
  /*** END of COUNT ENTRY TESTING CASE 1 **/
  /*** START of COUNT ENTRY TESTING CASE 2  **/

  behavior of "LinkedListBuffer"
  it should "Count Entry properly CASE 2  MIS ORDER CAPACITY FULL SITUATION" in {
    val buffer = createLinkedListBuffer(6);
    val c1, c2, c3, c4 ,c5 = new SolarInstallation()

    buffer.append(c1)
    buffer.append(c2)
    buffer.append(c4)
    buffer.append(c1)
    buffer.append(c2)
    buffer.append(c2)

    {
      val count: Int = buffer.countEntry(c3)
      assert(count == 0)
    }

    {
      val count: Int = buffer.countEntry(c2)
      assert(count == 3)
      assert(buffer.length == 6)
      assert(buffer.apply(0) == c1)
      assert(buffer.apply(buffer.length-1) == c2)

    }

    {
      val count: Int = buffer.countEntry(c1)
      assert(count == 2)
      assert(buffer.length == 6)
      assert(buffer.apply(0) == c1)
      assert(buffer.apply(buffer.length-1) == c2)

    }

    buffer.append(c5)

    {
      val count: Int = buffer.countEntry(c5)
      assert(count == 1)
      assert(buffer.length == 6)
      assert(buffer.apply(0) == c2)
      assert(buffer.apply(buffer.length-1) == c5)

    }
    {
      val count: Int = buffer.countEntry(c1)
      assert(count == 1)
      assert(buffer.length == 6)
      assert(buffer.apply(0)== c2)
      assert(buffer.apply(buffer.length-1) == c5)

    }
  }
  /*** END of COUNT ENTRY TESTING CASE 2**/
    /*** END of COUNT ENTRY TESTING**/


  /*** START of APPLY TESTING**/
    /*** START of APPLY TESTING CASE 1**/

  behavior of "LinkedListBuffer"
  it should "APPLY properly CASE 1" in {
    val buffer = createLinkedListBuffer(5);
    val ap1, ap2, ap3, ap4 ,ap5 = new SolarInstallation()

    buffer.append(ap1)
    buffer.append(ap2)
    buffer.append(ap4)
    buffer.append(ap3)
    buffer.append(ap5)


    {
      val app: SolarInstallation = buffer.apply(0)
      assert(app == ap1)
    }
    {
      val app: SolarInstallation = buffer.apply(1)
      assert(app == ap2)
    }
    {
      val app: SolarInstallation = buffer.apply(2)
      assert(app == ap4)
    }
    {
      val app: SolarInstallation = buffer.apply(3)
      assert(app == ap3)
    }
    {
      val app: SolarInstallation = buffer.apply(4)
      assert(app == ap5)
    }
  }


  /*** END of APPLY TESTING CASE 1**/
  /*** END of APPLY TESTING**/

  /*** START of UPDATE TESTING**/
  /*** START of UPDATE TESTING CASE 1**/

  behavior of "LinkedListBuffer"
  it should "Count UPDATE properly CASE 1  MIS ORDER CAPACITY FULL SITUATION" in {
    val buffer = createLinkedListBuffer(5);
    val p1, p2, p3, p4 ,p5, p6,p7 = new SolarInstallation()

    buffer.append(p1)
    buffer.append(p2)
    buffer.append(p4)
    buffer.append(p3)
    buffer.append(p5)


    {
      buffer.update(0,p6)
      assert( buffer.apply(0) == p6)
    }

    {
      buffer.update(2,p7)
      assert(buffer.apply(2) == p7)
    }
  }


  /*** END of UPDATE TESTING CASE 1**/
  /*** END of UPDATE TESTING**/


  /*** START of iterator TESTING**/
  /*** START of iterator TESTING CASE 1**/

  behavior of "LinkedListBuffer"
  it should "Count UPDATE properly CASE 1  MIS ORDER CAPACITY FULL SITUATION" in {
    val buffer = createLinkedListBuffer(5);
    val p1, p2, p3, p4 ,p5, p6,p7 = new SolarInstallation()

    buffer.append(p1)
    buffer.append(p2)
    buffer.append(p4)
    buffer.append(p3)
    buffer.append(p5)


    {
      buffer.update(0,p6)
      assert( buffer.apply(0) == p6)
    }

    {
      buffer.update(2,p7)
      assert(buffer.apply(2) == p7)
    }

  }


  /*** END of UPDATE TESTING CASE 1**/
  /*** END of UPDATE TESTING**/

  }