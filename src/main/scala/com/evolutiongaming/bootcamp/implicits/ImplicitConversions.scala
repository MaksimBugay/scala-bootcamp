package com.evolutiongaming.bootcamp.implicits

import scala.language.implicitConversions

object  ImplicitConversions extends App {

  // *Implicit conversions (advanced topic)*
  //
  // Implicit conversions is a very low level mechanism reserved for the library
  // users and mostly going away in Scala 3. You may never use it unless writing
  // an advanced library.
  //
  // The idea is quite simple. Whenever you need a type `A` somewhere, but only
  // `B` is available you can provide a method which converts one to another.
  //
  // Like this:

  def increment(value: Int): Int = value * 2

  implicit def stringToInt(value: String): Int = Integer.parseInt(value)
  increment("7")

  // Do you like the approach? Would you like to use it in your application?
  //
  // Turns out that implicit classes (extension methods) are internally
  // implemented as implicit conversions.
  //
  // Exercise (advanced):
  // Implement extension methods using implicit conversions instead of
  // implicit classses.

  // This should compile
  // println(7.pow(2))

  object LibC {
    trait Writer[A] {
      def write(a: A): String
    }
  }

  object LibA {
    trait Json[A] {
      def toJson(a: A): String
    }
  }

  object LibB {

    trait Serializable[A] {
      def serialize(a: A): String
    }
  }

  trait Foo

  implicit val fooJson: LibB.Serializable[Foo] = _.toString

  implicit def libBtoA[A](implicit s: LibB.Serializable[A]): LibA.Json[A] = ???

  implicit def libCtoA[A](implicit json: LibA.Json[A]): LibC.Writer[A] = ???

  def http200[A](a: A)(implicit json: LibC.Writer[A]): Unit = ???

  http200(new Foo {})
}
