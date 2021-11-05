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
package cse250.pa2

import org.scalatest.flatspec.AnyFlatSpec

class LSMIndexTest extends AnyFlatSpec {

  def lsmIndex: LSMIndex[Int, String] =
    new LSMIndex(100)


  behavior of "LSMIndex"
  it should "Support contains" in {
        val lsm = lsmIndex
    for (i <- 1 to 100) {
      lsm.insert(i, i.toString)
      assert(lsm.contains(i))
       assert(lsm(i).head == i.toString)

    }
   assert(lsm._levels(0).get.size == 100)
   assert(lsm._bufferElementsUsed == 0)

    for (i <- 1 to 100) {
    var  stein = i.toString + " second"
      lsm.insert(i, stein)
      assert(lsm.contains(i))
    }


    assert(lsm._levels(1).get.size == 200)
    assert(lsm._bufferElementsUsed == 0)



  }

}