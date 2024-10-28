package com.evolutiongaming.bootcamp.typeclass.v3_typeclass

import com.evolutiongaming.bootcamp.akka.actors.CommunicationPatterns.forwardDemo

final case class Json(s: String) { // simplified representation of JSON
  override def toString: String = s
}

object OOPJson extends App {

  trait Jsonable {
    def toJson: Json
  }

  def prettyPrint(jsonable: Jsonable): Unit = println(jsonable.toJson)

  final case class Game(id: Int) extends Jsonable {
    def toJson: Json = Json(s"""{"id": $id}""")
  }

  prettyPrint(Game(123))
}

// What if Game is defined in a library?

object FPJson extends App {

  trait Jsonable[A] {
    def toJson(entity: A): Json
  }

  def prettyPrint[A](a: A)(implicit jsonable: Jsonable[A]): Unit = println(jsonable.toJson(a))

  final case class Game(id: Int)

  implicit val gameJsonable: Jsonable[Game] = new Jsonable[Game] {
    def toJson(game: Game): Json = Json(s"""{"id": ${game.id}}""")
  }

  prettyPrint(Game(123))

  object InstancesTask {

    final case class Player(id: Int, name: String)

    implicit val playerJsonable: Jsonable[Player] = ???

    implicit val intJsonable: Jsonable[Int] = (entity: Int) => Json(entity.toString)

    implicit val optionIntJsonable: Jsonable[Option[Int]] = new Jsonable[Option[Int]] {

      override def toJson(entity: Option[Int]): Json = entity match {
        case Some(innerValue) => intJsonable.toJson(innerValue)
        case None => Json("null")
      }
    }
  }

  object GenericImplicitsTask {

    implicit def optionJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[Option[A]] =
      new Jsonable[Option[A]] {
        def toJson(entity: Option[A]): Json =
          entity match {
            case Some(value) => jsonableA.toJson(value)
            case None => Json("null")
          }
      }

    implicit def listJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[List[A]] = new Jsonable[List[A]] {

      override def toJson(entity: List[A]): Json = {
        val jsonElements = entity.map(jsonableA.toJson).map(_.toString)
        Json(s"[${jsonElements.mkString(",")}]")
      }
    }
  }

  object SingleAbstractMethod {

    implicit val before: Jsonable[Game] = new Jsonable[Game] {
      def toJson(game: Game): Json = Json(s"""{"id": ${game.id}}""")
    }

    implicit val after: Jsonable[Game] = game => Json(s"""{"id": ${game.id}}""")
  }

  object ContextBound {

    def prettyPrintBefore[A](a: A)(implicit jsonable: Jsonable[A]): Unit = println(jsonable.toJson(a))

    def prettyPrintAfter[A: Jsonable](a: A): Unit = println(
      implicitly[Jsonable[A]].toJson(a)
    )
  }

  object Summoner {

    object Jsonable {
      def apply[A](implicit instance: Jsonable[A]): Jsonable[A] = instance
    }

    def prettyPrintBefore[A: Jsonable](a: A): Unit = {
      val jsonable = Jsonable[A]
      println(jsonable.toJson(a))
    }

    def prettyPrintWithSummoner[A: Jsonable](a: A): Unit =
      println(Jsonable[A].toJson(a))
  }

  object Syntax {

    object Jsonable {
      def apply[A](implicit instance: Jsonable[A]): Jsonable[A] = instance
    }

    def prettyPrintBefore[A: Jsonable](a: A): Unit = println(Jsonable[A].toJson(a))

    object JsonableSyntax {
      implicit class JsonableOps[A](x: A) {
        def toJson(implicit jsonable: Jsonable[A]): Json = jsonable.toJson(x)
      }
    }

    import JsonableSyntax._

    def prettyPrintWithSyntax[A: Jsonable](a: A): Unit = println(a.toJson)
  }
}

object FPJsonSugared extends App {

  // Typeclass Definition
  trait Jsonable[T] {
    def toJson(entity: T): Json
  }

  // Typeclass Summoner
  object Jsonable {
    def apply[A](implicit instance: Jsonable[A]): Jsonable[A] = instance
  }

  // Typeclass Syntax
  object JsonableSyntax {
    implicit class JsonableOps[A](val x: A) extends AnyVal {
      def toJson(implicit jsonable: Jsonable[A]): Json = jsonable.toJson(x)
    }
  }

  final case class Game(id: Int)

  // Typeclass Instance
  implicit val gameJsonable: Jsonable[Game] = (game: Game) => Json(s"""{"id": ${game.id}}""")

  import JsonableSyntax._

  // This method can be called only if Typeclass Instance of Jsonable exists (and visible) for A
  def prettyPrint[A: Jsonable](a: A): Unit = println(a.toJson)

  prettyPrint(Game(123))
}

//object FPJsonMacros extends App {
//  import simulacrum._
//
//  @typeclass trait Jsonable[T] {
//    def toJson(entity: T): Json
//  }
//
//  import Jsonable.ops._
//  def prettyPrint[A: Jsonable](a: A): Unit = println(a.toJson)
//
//  final case class Game(id: Int)
//
//  implicit val gameJsonable: Jsonable[Game] = (game: Game) => Json(s"""{"id": ${game.id}}""")
//
//  prettyPrint(Game(123))
//}

object HashCodeTask {

  trait HashCode[T] { // Turn me into TypeClass
    def hash(entity: T): Int
  }

  /*object HashCode {
    def apply[A](implicit instance: HashCode[A]): HashCode[A] = instance
  }*/

  object HashCodeSyntax {
    implicit class HashCodeOps[A](val x: A) extends AnyVal {
      def hash(implicit hashCodeExtractor: HashCode[A]): Int = hashCodeExtractor.hash(x)
    }
  }

  implicit val stringHashCode: HashCode[String] = (str: String) => str.hashCode

  import HashCodeSyntax._

  final def main(args: Array[String]): Unit = {
    println("abc".hash)
    println("abc".hashCode)
  }
}
