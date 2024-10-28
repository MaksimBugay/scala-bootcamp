package com.evolutiongaming.bootcamp.typeclass.v3_typeclass

import cats.data.NonEmptyList
import com.evolutiongaming.bootcamp.typeclass.Manual.Semigroup
import com.evolutiongaming.bootcamp.typeclass.v3_typeclass.TypeClassesExamples.Semigroupal

import scala.::

object TypeClassesExamples extends App {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  // 1.2. Implement Semigroup for Long, String
  implicit val semigroupLong: Semigroup[Long] = _ + _

  implicit val semigroupString: Semigroup[String] = _ + _

  // 1.3. Implement combineAll(list: List[A]) for non-empty lists
  def combineAll[A: Semigroup](list: NonEmptyList[A]): A = {
    list.reduceLeft(implicitly[Semigroup[A]].combine)
  }

  combineAll(NonEmptyList.of(1L, 2L, 3L)) == 6

  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists

  // combineAll(List(1, 2, 3), 0) == 6
  // combineAll(List(), 1) == 1

  // 2. Monoid
  // 2.1. Implement Monoid which provides `empty` value (like startingElement in previous example) and extends Semigroup
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  // 2.2. Implement Monoid for Long, String
  implicit val monoidLong: Monoid[Long] = new Monoid[Long] {
    def combine(x: Long, y: Long): Long = implicitly[Semigroup[Long]].combine(x, y)

    def empty: Long = 0L
  }

  implicit val monoidString: Monoid[String] = new Monoid[String] {
    def combine(x: String, y: String): String = implicitly[Semigroup[String]].combine(x, y)

    def empty: String = ""
  }

  // 2.3. Implement combineAll(list: List[A]) for all lists
  def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = {
    list.foldLeft(monoid.empty)(monoid.combine)
  }

  // combineAll(List(1, 2, 3)) == 6

  // 2.4. Implement Monoid for Option[A]
  implicit def optionMonoid[A](implicit semigroupA: Semigroup[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    def empty: Option[A] = None

    def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(a), Some(b)) => Some(semigroupA.combine(a, b))
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(b)
      case (None, None) => None
    }
  }

  // combineAll(List(Some(1), None, Some(3))) == Some(4)
  // combineAll(List(None, None)) == None
  // combineAll(List()) == None

  // 2.5. Implement Monoid for Function1 (for result of the function)
  implicit def function1Monoid[A, B](implicit monoidB: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    // The "empty" function always returns monoidB.empty
    def empty: A => B = _ => monoidB.empty

    // The "combine" function combines the results of applying both functions to an argument
    def combine(f: A => B, g: A => B): A => B = a => monoidB.combine(f(a), g(a))
  }

  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))        === (a: String) => (a.length + a.toInt)
  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))("123") === 126

  // 3. Functor
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]
  }

  object FunctorSyntax {
    implicit class FunctorOps[F[_], A](fa: F[A]) {
      def map[B](f: A => B)(implicit functor: Functor[F]): F[B] = {
        functor.map(fa)(f)
      }
    }
  }
  import FunctorSyntax._

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def functionFunctor[T]: Functor[T => *] = new Functor[T => *] {
    override def map[A, B](fa: T => A)(f: A => B): T => B = t => f(fa(t))
  }

  Option(1).map(_ + 1)
  val ff: String => Int =
    ((f: String) => f.toInt).map(_ + 1)

  def fMap(a: Int, b: String): Long = ???
  def foo(f: Int => Long): Int = ???
  foo(fMap(_, "blabla"))

  // 3.1. Implement Functor for Map values
  implicit def mapStringFunctor: Functor[Map[String, *]] = new Functor[Map[String, *]] {
    override def map[A, B](fa: Map[String, A])(f: A => B): Map[String, B] = {
      fa.map { case (key, value) => key -> f(value) }
    }
  }

  implicit def mapLongFunctor: Functor[Map[Long, *]] = new Functor[Map[Long, *]] {
    override def map[A, B](fa: Map[Long, A])(f: A => B): Map[Long, B] = {
      fa.view.mapValues(f).toMap
    }
  }

  // 4. Semigroupal
  // 4.1. Semigroupal provides `product` method,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)
  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  //Option[Int] + Option[Int] == Option[Int]

  // 4.2. Implement Summoner for Semigroupal

  // 4.3. Implement Syntax for Semigroupal, so later you'll be able to do:
  // (Option(1) product Option(2)) == Some((1, 2))
  object SemigroupalSyntax {
    implicit class SemigroupalOps[F[_], A](fa: F[A]) {
      def product[B](fb: F[B])(implicit sg: Semigroupal[F]): F[(A, B)] = {
        sg.product(fa, fb)
      }
    }
  }

  import SemigroupalSyntax._

  // 4.4. Implement Semigroupal for Option
  implicit val optionSemigroupal: Semigroupal[Option] = new Semigroupal[Option] {
    def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
      (fa, fb) match {
        case (Some(a), Some(b)) => Some((a, b))
        case _                  => None
      }
  }

  // 4.5. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]
  /*implicit class Tuple2Ops[F[_], A, B](tuple: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R)(implicit functor: Functor[F], semigroupal: Semigroupal[F]): F[R] = {
      val (fa, fb) = tuple
      semigroupal.product(fa, fb).map { case (a, b) => f(a, b) }
    }
  }*/

  implicit class Tuple2Ops[F[_]: Semigroupal: Functor, A, B](tuple: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R): F[R] = {
      (tuple._1 product tuple._2)
        .map(f.tupled)
    }
  }

   (Option(1), Option(2)).mapN(_ + _) == Some(3)
   (Option(1), None).mapN(_ + _)      == None

  // 4.6. Implement Semigroupal for Map

  // (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }
  //pure(1) == Some(1)
  //pure(1) == List(1)

  object Applicative {
    def apply[F[_] : Applicative]: Applicative[F] = implicitly
  }

  implicit class ApplicativeValueOps[F[_] : Applicative, A](a: A) {
    def pure: F[A] = Applicative[F].pure(a)
  }

  // 5.1. Implement Applicative for Option, Either
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def pure[A](x: A): Option[A] = Some(x)

    // Implementing map for Option
    def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      Functor[Option].map(fa)(f)

    // Implementing product for Option
    def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = Semigroupal[Option].product(fa, fb)
  }

  implicit def eitherApplicative[E]: Applicative[Either[E, *]] = new Applicative[Either[E, *]] {
    def pure[A](x: A): Either[E, A] = Right(x)

    // Implementing map for Either
    def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    // Implementing product for Either
    def product[A, B](fa: Either[E, A], fb: Either[E, B]): Either[E, (A, B)] = (fa, fb) match {
      case (Right(a), Right(b)) => Right((a, b))
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    }
  }

  // 5.2. Implement `traverse` function
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    // Fold over the list, starting with the `Some(Nil)` as the initial accumulator
    as.foldRight(Option(List.empty[B])) { (a, acc) =>
      // For each element, apply the function and combine the results
      f(a) match {
        case Some(b) => acc.map(bs => b :: bs) // If `f(a)` is `Some`, prepend to the accumulator
        case None    => None // If `f(a)` is `None`, the entire result is `None`
      }
    }
  }

  // traverse(List(1, 2, 3)) { i =>
  //   Option.when(i % 2 == 1)(i)
  // } == None

  // traverse(List(1, 2, 3)) { i =>
  //   Some(i + 1)
  // } == Some(List(2, 3, 4))

  // 5.3. Implement `traverseA` for all Applicatives instead of Option
  def traverseA[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    val applicative = implicitly[Applicative[F]]

    // Start with the pure empty list
    as.foldRight(applicative.pure(List.empty[B])) { (a, acc) =>
      // Use product and map to combine the current element with the accumulated results
      applicative.product(f(a), acc).map { case (b, bs) => b :: bs }
    }
  }

  // traverseA(List(1, 2, 3)) { i =>
  //   Either.cond(i % 2 == 1, i, "Error")
  // } == Left("Error")

  // traverseA(List(1, 2, 3)) { i =>
  //   Right(i + 1): Either[Int, Any]
  // } == Right(List(2, 3, 4))

  // Scala Typeclassopedia: https://github.com/lemastero/scala_typeclassopedia
}
