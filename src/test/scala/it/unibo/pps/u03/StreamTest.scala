package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.{assertEquals, *}
import u03.Optionals.Optional.{Empty, Just}
import u03.Sequences.*
import Sequence.*
import u03.Streams.Stream.fibonacci

class StreamTest {

  import u03.Streams.*

  @Test def testTakeWhile() =
    val stream = Stream.iterate(0)(_ + 1)
    val expected = Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil())))))

    assertEquals(expected, Stream.toList(Stream.takeWhile(stream)(_ < 5)))

  @Test def testFill() = {
    val stream = Stream.toList(Stream.fill(3)("a"))
    val expected = Cons("a", Cons("a", Cons("a", Nil())))
    assertEquals(expected,  stream)
  }

  @Test def testFibonacci() =
    val fibList = Stream.toList(Stream.take(fibonacci)(5))
    val expected = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil())))))

    assertEquals(expected, fibList)

/* @Test def testOptionalInterleave() = ???

  @Test def testOptionalCycle() = ???*/


}
