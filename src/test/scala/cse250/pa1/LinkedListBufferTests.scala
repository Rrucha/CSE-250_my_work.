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
    e1.fields("name") = "5"
    e2.fields("name") = "6"
    e3.fields("name") = "7"
    e4.fields("name") = "8"
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
            e1.fields("name") = "5"
            e2.fields("name") = "6"
            e3.fields("name") = "7"
            e4.fields("name") = "8"
            e5.fields("name") = "9"

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
              assert(iterator.hasNext)
              assert( iterator.next() == e1)
              assert(iterator.hasNext)
              assert( iterator.next() == e2)
              assert(!iterator.hasNext)
              assert(buffer.length == 2)
              assert(buffer.apply(buffer.length-1) == e2)
            }

            buffer.append(e3)

            {
              val iterator = buffer.iterator
              assert(iterator.hasNext)
              assert( iterator.next() == e1)
              assert(iterator.hasNext)
              assert( iterator.next() == e2)
              assert(iterator.hasNext)
              assert( iterator.next() == e3)
              assert(!iterator.hasNext)
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
      e1.fields("name") = "5"
      e2.fields("name") = "6"
      e3.fields("name") = "7"
      e4.fields("name") = "8"
      e5.fields("name") = "9"

      // Iterator should start empty
            {
              val iterator = buffer.iterator
              assert(!iterator.hasNext)
            }

     assert( buffer.append(e2) == None)

            {
              val iterator = buffer.iterator
              assert(iterator.hasNext)
              assert( iterator.next() == e2)
              assert(!iterator.hasNext)
              assert(buffer.length == 1)
              assert(buffer.apply(buffer.length -1)== e2)
            }

     assert( buffer.append(e1) == None)

            {
              val iterator = buffer.iterator
              assert(iterator.hasNext)
              assert( iterator.next() == e2)
              assert(iterator.hasNext)
              assert( iterator.next() == e1)
              assert(!iterator.hasNext)
              assert(buffer.length == 2)
              assert(buffer.apply(buffer.length-1) == e1)
            }

    assert( buffer.append(e3) == None)

            {
              val iterator = buffer.iterator
              assert(iterator.hasNext)
              assert( iterator.next() == e2)
              assert(iterator.hasNext)
              assert( iterator.next() == e1)
              assert(iterator.hasNext)
              assert( iterator.next() == e3)
              assert(!iterator.hasNext)
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
      e1.fields("name") = "5"
      e2.fields("name") = "6"
      e3.fields("name") = "7"
      e4.fields("name") = "8"
      e5.fields("name") = "9"

      // Iterator should start empty
                {
                  val iterator = buffer.iterator
                  assert(!iterator.hasNext)
                }

     assert( buffer.append(e2) == None)

                {
                  val iterator = buffer.iterator
                  assert(iterator.hasNext)
                  assert( iterator.next() == e2)
                  assert(!iterator.hasNext)
                  assert(buffer.length == 1)
                  assert(buffer.apply(buffer.length-1) == e2)
                }

      assert(buffer.append(e1)==None)

              {
                val iterator = buffer.iterator
                assert(iterator.hasNext)
                assert( iterator.next() == e2)
                assert(iterator.hasNext)
                assert( iterator.next() == e1)
                assert(!iterator.hasNext)
                assert(buffer.length == 2)
                assert(buffer.apply(buffer.length-1) == e1)
              }


      assert( buffer.append(e3) == None)

              {
                val iterator = buffer.iterator
                assert(iterator.hasNext)
                assert( iterator.next() == e2)
                assert(iterator.hasNext)
                assert( iterator.next() == e1)
                assert(iterator.hasNext)
                assert( iterator.next() == e3)
                assert(!iterator.hasNext)
                assert(buffer.length == 3)
                assert(buffer.apply(buffer.length-1) == e3)
              }

    assert( buffer.append(e4) == None)

              {
                val iterator = buffer.iterator
                assert(iterator.hasNext)
                assert( iterator.next() == e2)
                assert(iterator.hasNext)
                assert( iterator.next() == e1)
                assert(iterator.hasNext)
                assert( iterator.next() == e3)
                assert(iterator.hasNext)
                assert( iterator.next() == e4)
                assert(!iterator.hasNext)
                assert(buffer.length == 4)
                assert(buffer.apply(buffer.length-1) == e4)
              }

             assert(buffer.append(e3) == Some(e2))

              {
                val iterator = buffer.iterator
                assert(iterator.hasNext)
                assert( iterator.next() == e1)
                assert(iterator.hasNext)
                assert( iterator.next() == e3)
                assert(iterator.hasNext)
                assert( iterator.next() == e4)
                assert(iterator.hasNext)
                assert( iterator.next() == e3)
                assert(!iterator.hasNext)
                assert(buffer.length == 4)
                assert(buffer.apply(buffer.length-1) == e3)
              }
      /*** END of APPEND TESTING CASE 3**/
    }

  /*** start of APPEND + REMOVE TESTING CASE 4**/

  behavior of "LinkedListBuffer"
  it should "Remove properly Case 4 Append n REPEAT REMOVE TAIL" in {
    val buffer = createLinkedListBuffer(7);
    val A, B, C, D ,E,F ,Z = new SolarInstallation()
    A.fields("name") = "1"
    B.fields("name") = "2"
    C.fields("name") = "3"
    D.fields("name") = "4"
    E.fields("name") = "5"
    F.fields("name") = "6"
    Z.fields("name") = "7"

    buffer.append(A) //0
    buffer.append(B) //1
    buffer.append(C)  //2
    buffer.append(D) //3
    buffer.append(D) //4
    buffer.append(E)  //5
    buffer.append(C)
    val Bool1 = buffer.remove(C)
    val Bool2: Boolean = buffer.remove(F)

    {
      assert(!Bool2)
    }

    {
      assert(Bool1)
      assert(buffer.length == 5)
      assert(buffer.apply(0) == A)
      assert(buffer.apply(buffer.length-1) == E)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == A) //0
      assert(iterator.hasNext)
      assert( iterator.next() == B) //1
      assert(iterator.hasNext)
      assert( iterator.next() == D)  //2
      assert(iterator.hasNext)
      assert( iterator.next() == D)  //3
      assert(iterator.hasNext)
      assert( iterator.next() == E)  //4
      assert(!iterator.hasNext)
    }





    buffer.append(Z)

    {
      assert(buffer.length == 6)
      assert(buffer.apply(buffer.length-1) == Z)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == A) //0
      assert(iterator.hasNext)
      assert( iterator.next() == B)  //1
      assert(iterator.hasNext)
      assert( iterator.next() == D) //2
      assert(iterator.hasNext)
      assert( iterator.next() == D) //3
      assert(iterator.hasNext)


      assert( iterator.next() == E) //4
      assert(iterator.hasNext)
      assert( iterator.next() == Z) //5
      assert(!iterator.hasNext)
    }
    buffer.append(D)

    {
      assert(buffer.length == 7)
      assert(buffer.apply(buffer.length-1) == D)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == A) //0
      assert(iterator.hasNext)
      assert( iterator.next() == B)  //1
      assert(iterator.hasNext)
      assert( iterator.next() == D) //2
      assert(iterator.hasNext)
      assert( iterator.next() == D) //3
      assert(iterator.hasNext)


      assert( iterator.next() == E) //4
      assert(iterator.hasNext)
      assert( iterator.next() == Z) //5
      assert(iterator.hasNext)
      assert( iterator.next() == D)
      assert(!iterator.hasNext)
    }
  }
  /*** END of APPEND +  REMOVE TESTING CASE 4**/
  /*** start of APPEND + REMOVE TESTING CASE 5**/

  behavior of "LinkedListBuffer"
  it should "Remove properly Case 5 Append n REPEAT REMOVE TAIL" in {
    val buffer = createLinkedListBuffer(6);
    val A, B, C, D ,E,F ,Z = new SolarInstallation()
    A.fields("name") = "1"
    B.fields("name") = "2"
    C.fields("name") = "3"
    D.fields("name") = "4"
    E.fields("name") = "5"
    F.fields("name") = "6"
    Z.fields("name") = "7"

    buffer.append(B)
    buffer.append(C)
    buffer.append(A)
    buffer.append(B)
    buffer.append(A)
    buffer.append(D)
    buffer.append(F)
    buffer.append(E)
    val Bool2: Boolean = buffer.remove(A)


    {
      assert(Bool2)
      assert(buffer.length == 4)
      assert(buffer.apply(0) == B)
      assert(buffer.apply(buffer.length-1) == E)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == B) //0
      assert(iterator.hasNext)
      assert( iterator.next() == D) //1
      assert(iterator.hasNext)
      assert( iterator.next() == F)  //2
      assert(iterator.hasNext)
      assert( iterator.next() == E)  //3

      assert(!iterator.hasNext)
    }


    buffer.append(Z)

    {
      assert(buffer.length == 5)
      assert(buffer.apply(buffer.length-1) == Z)
      val iterator = buffer.iterator

      assert(iterator.hasNext)
      assert( iterator.next() == B) //0
      assert(iterator.hasNext)
      assert( iterator.next() == D) //1
      assert(iterator.hasNext)
      assert( iterator.next() == F)  //2
      assert(iterator.hasNext)
      assert( iterator.next() == E)  //3
      assert(iterator.hasNext)
      assert( iterator.next() == Z) //5
      assert(!iterator.hasNext)
    }

  }
  /*** END of APPEND +  REMOVE TESTING CASE 5**/
  /*** start of APPEND + REMOVE TESTING CASE 6**/

  behavior of "LinkedListBuffer"
  it should "Remove properly Case 6 Append n REPEAT REMOVE TAIL" in {
    val buffer = createLinkedListBuffer(7);
    val A, B, C, D ,E,F ,Z = new SolarInstallation()
    A.fields("name") = "1"
    B.fields("name") = "2"
    C.fields("name") = "3"
    D.fields("name") = "4"
    E.fields("name") = "5"
    F.fields("name") = "6"
    Z.fields("name") = "7"

    buffer.append(A) //0
    buffer.append(B) //1
    buffer.append(C)  //2
    buffer.append(D) //3
    buffer.append(D) //4
    buffer.append(C)  //5
    buffer.append(E)
    val Bool1 = buffer.remove(C)


    {
      assert(Bool1)
      assert(buffer.length == 5)
      assert(buffer.apply(0) == A)
      assert(buffer.apply(buffer.length-1) == E)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == A) //0
      assert(iterator.hasNext)
      assert( iterator.next() == B) //1
      assert(iterator.hasNext)
      assert( iterator.next() == D)  //2
      assert(iterator.hasNext)
      assert( iterator.next() == D)  //3
      assert(iterator.hasNext)
      assert( iterator.next() == E)  //4
      assert(!iterator.hasNext)
    }

    buffer.append(Z)

    {
      assert(buffer.length == 6)
      assert(buffer.apply(buffer.length-1) == Z)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == A) //0
      assert(iterator.hasNext)
      assert( iterator.next() == B)  //1
      assert(iterator.hasNext)
      assert( iterator.next() == D) //2
      assert(iterator.hasNext)
      assert( iterator.next() == D) //3
      assert(iterator.hasNext)
      assert( iterator.next() == E) //4
      assert(iterator.hasNext)
      assert( iterator.next() == Z) //5
      assert(!iterator.hasNext)
    }
    buffer.append(D)

    {
      assert(buffer.length == 7)
      assert(buffer.apply(buffer.length-1) == D)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == A) //0
      assert(iterator.hasNext)
      assert( iterator.next() == B)  //1
      assert(iterator.hasNext)
      assert( iterator.next() == D) //2
      assert(iterator.hasNext)
      assert( iterator.next() == D) //3
      assert(iterator.hasNext)
      assert( iterator.next() == E) //4
      assert(iterator.hasNext)
      assert( iterator.next() == Z) //5
      assert(iterator.hasNext)
      assert( iterator.next() == D) //6
      assert(!iterator.hasNext)
    }
  }
  /*** END of APPEND +  REMOVE TESTING CASE 6**/

  /*** start of APPEND + REMOVE TESTING CASE 7**/

  behavior of "LinkedListBuffer"
  it should "Remove properly Case 7 Append n REPEAT REMOVE TAIL" in {
    val buffer = createLinkedListBuffer(7);
    val A, B, C, D ,E,F ,Z = new SolarInstallation()
    A.fields("name") = "1"
    B.fields("name") = "2"
    C.fields("name") = "3"
    D.fields("name") = "4"
    E.fields("name") = "5"
    F.fields("name") = "6"
    Z.fields("name") = "7"

    buffer.append(F) //0
    buffer.append(B) //1
    buffer.append(A)  //2
    buffer.append(D) //3
    buffer.append(D) //4
    buffer.append(F)  //5
    buffer.append(E)
    val Bool1 = buffer.remove(F)


    {
      assert(Bool1)
      assert(buffer.length == 5)
      assert(buffer.apply(0) == B)
      assert(buffer.apply(buffer.length-1) == E)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == B) //0
      assert(iterator.hasNext)
      assert( iterator.next() == A) //1
      assert(iterator.hasNext)
      assert( iterator.next() == D)  //2
      assert(iterator.hasNext)
      assert( iterator.next() == D)  //3
      assert(iterator.hasNext)
      assert( iterator.next() == E)  //4
      assert(!iterator.hasNext)
    }

    buffer.append(Z)

    {
      assert(buffer.length == 6)
      assert(buffer.apply(buffer.length-1) == Z)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == B) //0
      assert(iterator.hasNext)
      assert( iterator.next() == A)  //1
      assert(iterator.hasNext)
      assert( iterator.next() == D) //2
      assert(iterator.hasNext)
      assert( iterator.next() == D) //3
      assert(iterator.hasNext)
      assert( iterator.next() == E) //4
      assert(iterator.hasNext)
      assert( iterator.next() == Z) //5
      assert(!iterator.hasNext)
    }
    buffer.append(D)

    {
      assert(buffer.length == 7)
      assert(buffer.apply(buffer.length-1) == D)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == B) //0
      assert(iterator.hasNext)
      assert( iterator.next() == A)  //1
      assert(iterator.hasNext)
      assert( iterator.next() == D) //2
      assert(iterator.hasNext)
      assert( iterator.next() == D) //3
      assert(iterator.hasNext)
      assert( iterator.next() == E) //4
      assert(iterator.hasNext)
      assert( iterator.next() == Z) //5
      assert(iterator.hasNext)
      assert( iterator.next() == D) //6
      assert(!iterator.hasNext)
    }
  }
  /*** END of APPEND +  REMOVE TESTING CASE 7**/
          /*** END of APPEND TESTING**/

    /*** START of REMOVE TESTING**/
         /*** start of REMOVE TESTING CASE 1**/

    behavior of "LinkedListBuffer"
    it should "Remove properly Case 1 EVERYTHING PERFECT" in {
      val buffer = createLinkedListBuffer(4);
      val r1, r2, r3, r4 ,r5 = new SolarInstallation()
      r1.fields("name") = "5"
      r2.fields("name") = "6"
      r3.fields("name") = "7"
      r4.fields("name") = "8"
      r5.fields("name") = "9"

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

                val iterator = buffer.iterator
                assert(iterator.hasNext)
                assert( iterator.next() == r1)
                assert(iterator.hasNext)
                assert( iterator.next() == r2)
                assert(iterator.hasNext)
                assert( iterator.next() == r5)
                assert(!iterator.hasNext)
              }
    }
    /*** END of REMOVE TESTING CASE 1**/
    /*** start of REMOVE TESTING CASE 2**/
    behavior of "LinkedListBuffer"
    it should "Remove properly Case 2 REMOVE HEAD" in {
      val buffer = createLinkedListBuffer(4);
      val r21, r22, r23, r24 ,r25 = new SolarInstallation()
      r21.fields("name") = "5"
      r22.fields("name") = "6"
      r23.fields("name") = "7"
      r24.fields("name") = "8"
      r25.fields("name") = "9"

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

                val iterator = buffer.iterator
                assert(iterator.hasNext)
                assert( iterator.next() == r22)
                assert(iterator.hasNext)
                assert( iterator.next() == r24)
                assert(iterator.hasNext)
                assert( iterator.next() == r25)
                assert(!iterator.hasNext)
              }
    }
    /*** END of REMOVE TESTING CASE 2**/
    /*** start of REMOVE TESTING CASE 3**/

    behavior of "LinkedListBuffer"
    it should "Remove properly Case 3 REPEAT REMOVE TAIL" in {
      val buffer = createLinkedListBuffer(4);
      val r31, r32, r33, r34 ,r35 = new SolarInstallation()
      r31.fields("name") = "5"
      r32.fields("name") = "6"
      r33.fields("name") = "7"
      r34.fields("name") = "8"
      r35.fields("name") = "9"

        buffer.append(r35)
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
              assert(buffer.length == 2)
              assert(buffer.apply(buffer.length-1) == r34)
              val iterator = buffer.iterator
              assert(iterator.hasNext)
              assert( iterator.next() == r32)
              assert(iterator.hasNext)
              assert( iterator.next() == r34)
              assert(!iterator.hasNext)
            }

    }
         /*** END of REMOVE TESTING CASE 3**/

    /*** start of REMOVE TESTING CASE 4**/

  behavior of "LinkedListBuffer"
  it should "Remove properly Case 4 REMOVE REPEAT CPY" in {
    val buffer = createLinkedListBuffer(4);
    val r31, r32, r33, r34 ,r35 = new SolarInstallation()
    r31.fields("name") = "5"
    r32.fields("name") = "6"
    r33.fields("name") = "7"
    r34.fields("name") = "8"
    r35.fields("name") = "9"

    buffer.append(r31)
    buffer.append(r32)
    buffer.append(r31)
    buffer.append(r35)




    {
      val Bool: Boolean = buffer.remove(r31)
      assert(Bool)
      assert(buffer.length == 2)

      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == r32)
      assert(iterator.hasNext)
      assert( iterator.next() == r35)
      assert(!iterator.hasNext)
    }

    {
      val Bool = buffer.remove(r35)
      assert(Bool)
      assert(buffer.apply(buffer.length-1) == r32)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == r32)
      assert(!iterator.hasNext)
    }

  }

  /*** END of REMOVE TESTING CASE 5**/

  behavior of "LinkedListBuffer"
  it should "Remove properly Case 5 REMOVE TAIL" in {
    val buffer = createLinkedListBuffer(4);
    val r31, r32, r33, r34 ,r35 = new SolarInstallation()
    r31.fields("name") = "5"
    r32.fields("name") = "6"
    r33.fields("name") = "7"
    r34.fields("name") = "8"
    r35.fields("name") = "9"

    buffer.append(r31)
    buffer.append(r32)
    buffer.append(r34)
    buffer.append(r35)


    {
      val Bool: Boolean = buffer.remove(r35)
      assert(Bool)
      assert(buffer.length == 3)
      assert(buffer.apply(buffer.length-1) == r34)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == r31)
      assert(iterator.hasNext)
      assert( iterator.next() == r32)
      assert(iterator.hasNext)
      assert( iterator.next() == r34)
      assert(!iterator.hasNext)
    }

  }
  /*** END of REMOVE TESTING CASE 5**/

  /*** START of REMOVE TESTING CASE 6**/

  behavior of "LinkedListBuffer"
  it should "Remove properly Case 6  CAPACITY FULL REMOVE AND APPEND AFTER THAT" in {
    val buffer = createLinkedListBuffer(4);
    val r31, r32, r33, r34 ,r35, r3 = new SolarInstallation()
    r31.fields("name") = "5"
    r32.fields("name") = "6"
    r33.fields("name") = "7"
    r34.fields("name") = "8"
    r35.fields("name") = "9"
    r3.fields("name") = "3"
    buffer.append(r31) // 0
    buffer.append(r32) //1
    buffer.append(r34)  //2
    buffer.append(r35) //3
    buffer.append(r32) //0
    buffer.append(r31) //1
    val bool=  buffer.remove(r35)
    buffer.append(r3) //3


    {
      assert(bool)
      assert(buffer.length == 4)
      assert(buffer.apply(buffer.length-1) == r3)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == r34)
      assert(iterator.hasNext)
      assert( iterator.next() == r32)
      assert(iterator.hasNext)
      assert( iterator.next() == r31)
      assert(iterator.hasNext)
      assert( iterator.next() == r3)
      assert(!iterator.hasNext)
    }

  }
  /*** END of REMOVE TESTING CASE 6**/
  /*** START of REMOVE TESTING CASE 7**/

  behavior of "LinkedListBuffer"
  it should "Remove properly Case 7  CAPACITY FULL REMOVE AND APPEND AFTER THAT" in {
    val buffer = createLinkedListBuffer(4);
    val r31, r32, r33, r34 ,r35, r36 = new SolarInstallation()
    r31.fields("name") = "5"
    r32.fields("name") = "6"
    r33.fields("name") = "7"
    r34.fields("name") = "8"
    r35.fields("name") = "9"
    r36.fields("name") = "1"

    buffer.append(r31) //0
    buffer.append(r32) //1
    buffer.append(r34) //2
    buffer.append(r35) //3

    buffer.append(r36) //4  //0
    val bool=  buffer.remove(r35)
    buffer.append(r31) // 0
    buffer.append(r34) // 1
    buffer.append(r35) // 2


    {
      assert(bool)
      assert(buffer.length == 4)
      assert(buffer.apply(buffer.length-1) == r35)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == r36)
      assert(iterator.hasNext)
      assert( iterator.next() == r31)
      assert(iterator.hasNext)
      assert( iterator.next() == r34)
      assert(iterator.hasNext)
      assert( iterator.next() == r35)
      assert(!iterator.hasNext)
    }

  }
  /*** END of REMOVE TESTING CASE 7**/
  /*** start of REMOVE TESTING CASE 8**/

  behavior of "LinkedListBuffer"
  it should "Remove properly Case 8 EVERYTHING PERFECT" in {
    val buffer = createLinkedListBuffer(4);
    val r1, r2, r3, r4 ,r5 = new SolarInstallation()
    r1.fields("name") = "5"
    r2.fields("name") = "6"
    r3.fields("name") = "7"
    r4.fields("name") = "8"
    r5.fields("name") = "9"

    buffer.append(r1)

    {
      val Bool = buffer.remove(r3)
      assert(!Bool)
    }
    {
      assert(buffer.apply(0) == r1 )
    }
    {
      val Bool: Boolean = buffer.remove(r1)
      assert(Bool)
      assert(buffer.length == 0)

      val iterator = buffer.iterator
      assert(!iterator.hasNext)

    }

  }
  /*** END of REMOVE TESTING CASE 8**/

  /*** START of REMOVE TESTING CASE 9**/

  behavior of "LinkedListBuffer"
  it should "Remove properly Case 9  exceed CAPACITY " in {
    val buffer = createLinkedListBuffer(4);
    val r31, r32, r33, r34 ,r35, r3 = new SolarInstallation()
    r31.fields("name") = "5"
    r32.fields("name") = "6"
    r33.fields("name") = "7"
    r34.fields("name") = "8"
    r35.fields("name") = "9"
    r3.fields("name") = "3"
    buffer.append(r31) // 0
    buffer.append(r32) //1
    buffer.append(r34)  //2
    buffer.append(r35) //3
    buffer.append(r32) //0
    buffer.append(r31) //1
    val bool=  buffer.remove(r35)


    {
      assert(bool)
      assert(buffer.length == 3)
      assert(buffer.head == r34)
      assert(buffer.apply(buffer.length-1) == r31)
      assert(buffer.apply(0) == r34)
      assert(buffer.apply(1) == r32)
      assert(buffer.apply(2) == r31)
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert( iterator.next() == r34)
      assert(iterator.hasNext)
      assert( iterator.next() == r32)
      assert(iterator.hasNext)
      assert( iterator.next() == r31)
      assert(!iterator.hasNext)

    }

  }
  /*** END of REMOVE TESTING CASE 9**/

  /*** END of REMOVE TESTING**/
  /*** START of COUNT ENTRY TESTING**/
        /*** START of COUNT ENTRY TESTING CASE 1  **/

    behavior of "LinkedListBuffer"
    it should "Count Entry properly CASE1 " in {
      val buffer = createLinkedListBuffer(4);
      val c1, c2, c3, c4 ,c5 = new SolarInstallation()
      c1.fields("name") = "5"
      c2.fields("name") = "6"
      c3.fields("name") = "7"
      c4.fields("name") = "8"
      c5.fields("name") = "9"

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
              assert(buffer.length == 4)
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
    c1.fields("name") = "5"
    c2.fields("name") = "6"
    c3.fields("name") = "7"
    c4.fields("name") = "8"
    c5.fields("name") = "9"

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
                assert(buffer.head == c1)
                assert(buffer.apply(buffer.length-1) == c2)

              }

              {
                val count: Int = buffer.countEntry(c1)
                assert(count == 2)
                assert(buffer.length == 6)
                assert(buffer.head == c1)
                assert(buffer.apply(buffer.length-1) == c2)

              }

              buffer.append(c5)

              {
                val count: Int = buffer.countEntry(c5)
                assert(count == 1)
                assert(buffer.length == 6)
                assert(buffer.head == c2)
                assert(buffer.apply(buffer.length-1) == c5)

              }
              {
                val count: Int = buffer.countEntry(c1)
                assert(count == 1)
                assert(buffer.length == 6)
                assert(buffer.head == c2)
                assert(buffer.apply(buffer.length-1) == c5)

              }
  }
  /*** END of COUNT ENTRY TESTING CASE 2**/
  /*** START of COUNT ENTRY TESTING CASE 3  **/

  behavior of "LinkedListBuffer"
  it should "Count Entry properly CASE 3 UPDATE + MIS ORDER CAPACITY FULL SITUATION" in {
    val buffer = createLinkedListBuffer(6);
    val c1, c2, c3, c4 ,c5 = new SolarInstallation()
    c1.fields("name") = "5"
    c2.fields("name") = "6"
    c3.fields("name") = "7"
    c4.fields("name") = "8"
    c5.fields("name") = "9"

    buffer.append(c1)
    buffer.append(c2)
    buffer.append(c4)
    buffer.append(c1)
    buffer.append(c2)
    buffer.append(c2)
    buffer.update(0,c3)

    {
      val count: Int = buffer.countEntry(c3)
      assert(count == 1)
    }

    {
      val count: Int = buffer.countEntry(c2)
      assert(count == 3)
      assert(buffer.length == 6)
      assert(buffer.head == c3)
      assert(buffer.apply(buffer.length-1) == c2)

    }

    {
      val count: Int = buffer.countEntry(c1)
      assert(count == 1)
      assert(buffer.length == 6)
      assert(buffer.head == c3)
      assert(buffer.apply(buffer.length-1) == c2)

    }

    buffer.append(c5)

    {
      val count: Int = buffer.countEntry(c5)
      assert(count == 1)
      assert(buffer.length == 6)
      assert(buffer.head == c2)
      assert(buffer.apply(buffer.length-1) == c5)

    }
    {
      val count: Int = buffer.countEntry(c1)
      assert(count == 1)
      assert(buffer.length == 6)
      assert(buffer.head == c2)
      assert(buffer.apply(buffer.length-1) == c5)

    }
  }
  /*** END of COUNT ENTRY TESTING CASE 3**/
    /*** END of COUNT ENTRY TESTING**/


  /*** START of APPLY TESTING**/
    /*** START of APPLY TESTING CASE 1**/

  behavior of "LinkedListBuffer"
  it should "APPLY properly CASE 1" in {
    val buffer = createLinkedListBuffer(5);
    val ap1, ap2, ap3, ap4 ,ap5 = new SolarInstallation()
    ap1.fields("name") = "5"
    ap2.fields("name") = "6"
    ap3.fields("name") = "7"
    ap4.fields("name") = "8"
    ap5.fields("name") = "9"

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
    p1.fields("name") = "5"
    p2.fields("name") = "6"
    p3.fields("name") = "7"
    p4.fields("name") = "8"
    p5.fields("name") = "9"
    p6.fields("name") = "1"
    p7.fields("name") = "2"

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
  /*** START of UPDATE TESTING CASE 2**/

  behavior of "LinkedListBuffer"
  it should "Count UPDATE properly CASE 2  MIS ORDER CAPACITY FULL SITUATION" in {
    val buffer = createLinkedListBuffer(5);
    val p1, p2, p3, p4 ,p5, p6,p7 = new SolarInstallation()
    p1.fields("name") = "5"
    p2.fields("name") = "6"
    p3.fields("name") = "7"
    p4.fields("name") = "8"
    p5.fields("name") = "9"
    p6.fields("name") = "1"
    p7.fields("name") = "2"

    buffer.append(p1)
    buffer.append(p2)
    buffer.append(p4)
    buffer.append(p3)
    buffer.append(p5)
    buffer.append(p4)

    {
      buffer.update(0,p6)
      assert( buffer.apply(0) == p6)
    }

    {
      buffer.update(2,p7)
      assert(buffer.apply(2) == p7)
    }

  }


  /*** END of UPDATE TESTING CASE 2**/
  /*** END of UPDATE TESTING**/


  /*** START of iterator TESTING**/
  /*** START of iterator TESTING CASE 1**/

  behavior of "LinkedListBuffer"
  it should "Count iteratior properly CASE 1 " in {
    val buffer = createLinkedListBuffer(4);
    val p1, p2, p3, p4 ,p5, p6,p7 = new SolarInstallation()
    p1.fields("name") = "5"
    p2.fields("name") = "6"
    p3.fields("name") = "7"
    p4.fields("name") = "8"
    p5.fields("name") = "9"
    p6.fields("name") = "1"
    p7.fields("name") = "2"

                assert(buffer.append(p1) == None)
                assert(buffer.append(p2) == None)
                assert(buffer.append(p3) == None)
                assert(buffer.append(p4) == None)
                assert(buffer.append(p3) == Some(p1))
                assert(buffer.append(p5) == Some(p2))

              {
                val iter = buffer.iterator
                assert(iter.hasNext)
                assert(iter.next() == p3)
                assert(iter.hasNext)
                assert( iter.next() == p4)
                assert(iter.hasNext)
                assert( iter.next() == p3)

                iter.remove()

              }

              {
                val iter = buffer.iterator
                assert(iter.hasNext)
                assert(iter.next() == p3)
                assert(iter.hasNext)
                assert( iter.next() == p4)
                assert(iter.hasNext)
                assert( iter.next() == p5)
                assert(!iter.hasNext)
              }

  }


  /*** END of UPDATE TESTING CASE 2**/

  behavior of "LinkedListBuffer"
  it should "Count iteratior properly CASE 2 " in {
    val buffer = createLinkedListBuffer(4);
    val p1, p2, p3, p4 ,p5, p6,p7 = new SolarInstallation()
    p1.fields("name") = "5"
    p2.fields("name") = "6"
    p3.fields("name") = "7"
    p4.fields("name") = "8"
    p5.fields("name") = "9"
    p6.fields("name") = "1"
    p7.fields("name") = "2"

    assert(buffer.append(p1) == None)
    assert(buffer.append(p2) == None)
    assert(buffer.append(p3) == None)
    assert(buffer.append(p4) == None)
    assert(buffer.append(p7) == Some(p1))
    assert(buffer.append(p5) == Some(p2))

    {
      val iter = buffer.iterator
      assert(iter.hasNext)
      assert(iter.next() == p3)
      assert(iter.hasNext)
      assert( iter.next() == p4)
      assert(iter.hasNext)
      assert( iter.next() == p7)

      iter.remove()

    }

    {
      val iter = buffer.iterator
      assert(iter.hasNext)
      assert(iter.next() == p3)
      assert(iter.hasNext)
      assert( iter.next() == p4)
      assert(iter.hasNext)
      assert( iter.next() == p5)
      assert(!iter.hasNext)
    }

  }
  /*** END of UPDATE TESTING CASE 3**/
  behavior of "LinkedListBuffer"
  it should "Count iteratior properly CASE 3 " in {
    val buffer = createLinkedListBuffer(4);
    val p1, p2, p3, p4 ,p5, p6,p7 = new SolarInstallation()
    p1.fields("name") = "5"
    p2.fields("name") = "6"
    p3.fields("name") = "7"
    p4.fields("name") = "8"
    p5.fields("name") = "9"
    p6.fields("name") = "1"
    p7.fields("name") = "2"

    assert(buffer.append(p1) == None)
    assert(buffer.append(p2) == None)
    assert(buffer.append(p3) == None)
    assert(buffer.append(p4) == None)
    assert(buffer.append(p7) == Some(p1))
    assert(buffer.append(p5) == Some(p2))

    {
      val iter = buffer.iterator
      assert(iter.hasNext)
      assert(iter.next() == p3)
      assert(iter.hasNext)
      assert( iter.next() == p4)
      assert(iter.hasNext)
      assert( iter.next() == p7)
      assert(iter.hasNext)
      assert( iter.next() == p5)
      iter.remove()

    }

    {
      val iter = buffer.iterator
      assert(iter.hasNext)
      assert(iter.next() == p3)
      assert(iter.hasNext)
      assert( iter.next() == p4)
      assert(iter.hasNext)
      assert( iter.next() == p7)
      assert(!iter.hasNext)
    }

  }
  /*** END of UPDATE TESTING CASE 3**/

  /*** END of UPDATE TESTING CASE 4**/
  behavior of "LinkedListBuffer"
  it should "Count iteratior properly CASE 4 " in {
    val buffer = createLinkedListBuffer(4);
    val p1, p2, p3, p4 ,p5, p6,p7 = new SolarInstallation()
    p1.fields("name") = "5"
    p2.fields("name") = "6"
    p3.fields("name") = "7"
    p4.fields("name") = "8"
    p5.fields("name") = "9"
    p6.fields("name") = "1"
    p7.fields("name") = "2"

    assert(buffer.append(p1) == None)
    assert(buffer.append(p2) == None)
    assert(buffer.append(p3) == None)
    assert(buffer.append(p4) == None)
    assert(buffer.append(p7) == Some(p1))
    assert(buffer.append(p5) == Some(p2))

    {
      val iter = buffer.iterator
      assert(iter.hasNext)
      assert(iter.next() == p3)
      assert(iter.hasNext)
      iter.remove()

    }

    {
      val iter = buffer.iterator
      assert(iter.hasNext)
      assert( iter.next() == p4)
      assert(iter.hasNext)
      assert( iter.next() == p7)
      assert(iter.hasNext)
      assert( iter.next() == p5)
      assert(!iter.hasNext)
    }

  }
  /*** END of UPDATE TESTING CASE 4**/
  /*** END of UPDATE TESTING CASE 5**/
  behavior of "LinkedListBuffer"
  it should "Count iteratior properly CASE 5 " in {
    val buffer = createLinkedListBuffer(4);
    val p1, p2, p3, p4 ,p5, p6,p7 = new SolarInstallation()
    p1.fields("name") = "5"
    p2.fields("name") = "6"
    p3.fields("name") = "7"
    p4.fields("name") = "8"
    p5.fields("name") = "9"
    p6.fields("name") = "1"
    p7.fields("name") = "2"

    assert(buffer.append(p1) == None)
    assert(buffer.append(p2) == None)
    assert(buffer.append(p3) == None)
    assert(buffer.append(p4) == None)
    assert(buffer.append(p7) == Some(p1))
    assert(buffer.append(p5) == Some(p2))

    {
      val iter = buffer.iterator
      assert(iter.hasNext)
      assert(iter.next() == p3)
      assert(iter.hasNext)
      assert(iter.next() == p4)
      iter.remove()

    }

    {
      val iter = buffer.iterator
      assert(iter.hasNext)
      assert( iter.next() == p3)
      assert(iter.hasNext)
      assert( iter.next() == p7)
      assert(iter.hasNext)
      assert( iter.next() == p5)
      assert(!iter.hasNext)
    }

  }
  /*** END of UPDATE TESTING CASE 5**/
  /*** END of UPDATE TESTING**/

  }