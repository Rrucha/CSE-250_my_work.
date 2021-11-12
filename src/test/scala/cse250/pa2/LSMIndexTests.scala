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
    for (i <- 1 to 99) {
      lsm.insert(i, "1")
      assert(lsm.contains(i))
       assert(lsm.apply(i).head == "1")

    }
    lsm.insert(100, "1")

   assert(lsm._levels(0).get.size == 100)
   assert(lsm._bufferElementsUsed == 0)

    for (i <- 1 to 100) {
    var  stein = "1"
      lsm.insert(i, stein)
      assert(lsm.contains(i))
    }

    for (k <- 1 to 100){
      assert(lsm.apply(k)(1) == "1")
      assert(lsm.contains(k))
      assert(lsm.apply(k)(1) == "1")
    }
    val w = lsm.apply(100)
    assert(lsm._levels(0).isEmpty)
    assert(lsm._levels(1).get.size == 200)
    assert(lsm._bufferElementsUsed == 0)
    for (i <- 1 to 100) {
      var  stein = "1"
      lsm.insert(i, stein)
      assert(lsm.contains(i))
      assert(lsm.apply(i)(1) == "1")
    }
    val v = lsm.apply(100)
    assert(lsm._levels(0).get.size == 100)
    assert(lsm._levels(1).get.size == 200)
    for (i <- 1 to 100) {
      var  stein = "1"
      lsm.insert(i, stein)
      assert(lsm.contains(i))
      val j = lsm.apply(i)(1)
      assert(lsm.apply(i)(1) == "1")
    }

    val j = lsm.apply(100)


    assert(lsm._levels(2).get.size == 400)


    for (i <- 1 to 100) {
      var  stein = "1"
      lsm.insert(i, stein)
      assert(lsm.contains(i))
      assert(lsm.apply(i)(1) == "1")
    }
    val b = lsm.apply(100)

    assert(lsm._levels(0).get.size == 100)
    assert(lsm._levels(2).get.size == 400)
    for (i <- 1 to 100) {
      var  stein = "1"
      lsm.insert(i, stein)
      assert(lsm.contains(i))
      assert(lsm.apply(i)(1) == "1")
    }
    assert(lsm._levels(1).get.size == 200)
    assert(lsm._levels(2).get.size == 400)

    for (i <- 1 to 9) {
      var  stein = "1"
      lsm.insert(i, stein)
      assert(lsm.contains(i))
      assert(lsm.apply(i)(1) == "1")
    }
    val e = lsm.apply(9)

    for (i <- 10 to 100) {
      var  stein = "1"
      lsm.insert(i, stein)
      assert(lsm.contains(i))
      assert(lsm.apply(i)(1) == "1")
    }

    val d = lsm.apply(99)

    assert(lsm._levels(0).get.size == 100)
    assert(lsm._levels(1).get.size == 200)
    assert(lsm._levels(2).get.size == 400)

    for (i <- 1 to 100) {
      var  stein = "1"
      lsm.insert(i, stein)
      assert(lsm.contains(i))
      assert(lsm.apply(i)(1) == "1")
    }
    val f = lsm.apply(99)

    assert(lsm._levels(3).get.size == 800)
    for (i <- 1 to 100) {
      var  stein = "1"
      lsm.insert(i, stein)
      assert(lsm.contains(i))


      assert(lsm.apply(i)(1) == "1")
    }


    for (i <- 101 to 200) {
      var  stein = "1"
      lsm.insert(i, stein)
      assert(lsm.contains(i))
      val okkk = lsm.apply(i)

    }


    val q = lsm.apply(99)

    assert(lsm._levels(1).get.size == 200)
    assert(lsm._levels(3).get.size == 800)

    for (i <- 1 to 600) {
      var  stein = "1"
      lsm.insert(i, stein)
      assert(lsm.contains(i))
      val okkk = lsm.apply(i)
    }

    assert(lsm._levels(4).get.size == 1600)
    val g = lsm.apply(99)


  }


}