package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.{assertEquals, *}
import u03.Optionals.Optional.{Empty, Just}
import u03.Sequences.*
import Sequence.*
import u03.Streams.Stream.{cons, fibonacci}

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

  @Test def testOptionalInterleave() = {
    val s1 = cons(1, cons(3, cons(5, Stream.empty())))
    val s2 = cons(2, cons(4, cons(6, cons(8, cons(10, Stream.empty())))))
    //scritti in questo modo perché non ho trovato la funzione fromList

    val expected =  Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(8, Cons(10, Nil()))))))))
    assertEquals(expected, Stream.toList(Stream.interleave(s1, s2)))

    val q1 = cons(1, cons(3, cons(5, Stream.empty())))
    val q2 = cons(9, Stream.empty())
    val expected2 = Cons(1, Cons(9, Cons(3, Cons(5, Nil()))))
    assertEquals(expected2, Stream.toList(Stream.interleave(q1, q2)))

  }

  @Test def testCycle() = {
    val seq = Cons(1, Cons(2, Cons(3, Nil())))
    val cycledStream = Stream.cycle(seq)
    val resultList = Stream.toList(Stream.take(cycledStream)(7))
    val expected = Cons(1, Cons(2, Cons(3, Cons(1, Cons(2, Cons(3, Cons(1, Nil())))))))

    assertEquals(expected, resultList)
  }


}
