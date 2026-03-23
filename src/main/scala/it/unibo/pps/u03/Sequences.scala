package u03

import u03.Optionals.Optional
import u03.Sequences.Person.Teacher

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03
    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */

    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match {
      case Cons(h,t) if n > 0 => skip(t)(n-1)
      case Cons(h,t) => s //sottintendo "if n == 0"
      case _ => Nil()
    }

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */

    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match {
      case (Cons(h1,t1), Cons(h2,t2)) => Cons((h1,h2), zip(t1,t2))
      case _ => Nil()
    }

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match {
      case Cons(h1, t1) => Cons(h1, concat(t1, s2))
      case _ => s2
    }

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] = {

      def helper(s: Sequence[A], res: Sequence[A]) : Sequence[A] = s match {
        case Cons(h,t) => helper(t, Cons(h, res))
        case _ => res
      }

      helper(s, Nil())
    }

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match {
      case Cons(h,t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()
    }

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] = s match {

      case Cons(h, t) =>
        def helper(s: Sequence[Int], actual : Int) : Optional[Int] = s match {

          case Cons(h,t) if (h > actual) => helper(t, actual)
          case Cons(h,t) => helper(t, h)
          case _ => Optional.Just(actual)
        }
        helper(s, h)

      case _ => Optional.Empty()

    }

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] = s match {
      case Cons(h1, Cons(_, t)) => Cons(h1, evenIndices(t))
      case Cons(h1, Nil()) => Cons(h1, Nil())
      case _ => Nil()

    }

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match {
      case Cons(h,t) if elem == h => true
      case Cons(h,t) => contains(t)(elem)
      case _ => false
    }

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] = s match {
      case Cons(h, t) => Cons(h, distinct(filter(t)(_ != h)))
      case _ => Nil()
    }

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = s match {
      case Cons(h, t) => ???

      case _ => Nil()
    }

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = s match {
      case Cons(h,t) => partition(t)(pred) match {
        case (listaGood, listaBad) if pred(h) => (Cons(h,listaGood), listaBad)
        case (listaGood, listaBad) => (listaGood, Cons(h, listaBad))
      }

      case _ => (s, s)

    }


    /* ex. 1 */

    def teacherCourses(s: Sequence[Person]): Sequence[String] = {
      //SOLUZIONE CON FILTER + MAP
      val onlyTeachers = filter(s)(p => p match {
        case Person.Teacher(_,_) => true
        case _ => false
      })
      //non c'è bisogno di "p => p match (si può fare come sotto)
      //ma lo tengo come esempio iniziale per ricordarmi

      map(onlyTeachers) {
        case Person.Teacher(_, course) => course
        case _ => ""
      }


      //SOLUZIONE CON FLATMAP
      /*flatMap(s)(p => p match {
        case Person.Teacher(_, course) => Cons(course, Nil())
        case Person.Student(_, _) => Nil()
      })*/
    }

    /* ex. 2 */

    def foldLeft[A, B](list: Sequence[A])(default: B)(op: (B,A) => B) : B = list match {
      case Cons(h, t) => foldLeft(t)(op(default,h))(op)
      case Nil() => default
    }

    /* ex. 3 */

    def getNumberOfCourses(s: Sequence[Person]) : Int = {
      val onlyTeachers = filter(s) {
        case Person.Teacher(_,_) => true
        case _ => false
      }
      val onlyOnes = map(onlyTeachers)(onlyTeachers => 1)
      foldLeft(onlyOnes)(0)(_ + _)
    }


@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
