package com.evolutiongaming.bootcamp.hkt

object Generics {

  def secondElementOfIntList(xs: List[Int]): Option[Int] = xs match {
    case _ :: x :: _ => Some(x)
    case _ => None
  }

  def main(args: Array[String]): Unit = {
    println(secondElementOfIntList(List(1,2,3)));
  }

  sealed trait Index
  object Index {
    case object X extends Index
    case object Y extends Index
    case object Z extends Index
  }
  case class Triple[+A](x: A, y: A, z: A) {
    def toList: List[A] = List(x, y, z)

    // exercise 1 :  implement
    def zip[B](other: Triple[B]): Triple[(A, B)] = Triple((x, other.x), (y, other.y), (z, other.z))

    //exercise 3 (hard) : fix the definition and implement
    def set[B >: A](index: Index, value: B): Triple[B] = index match {
      case Index.X => Triple(value, y, z)
      case Index.Y => Triple(x, value, z)
      case Index.Z => Triple(x, y, value)
    }

    def withFirst[A1 >: A](a: A1): Triple[A1] = Triple(a, y, z)

    def map[B](f: A => B): Triple[B] = ???
  }

  val t: Triple[Any] = Triple("a", "b", 1)


  object Triple {

    // exercise 2 : implement
    def fromList[A](elements: List[A]): Option[Triple[A]] = elements match {
      case x :: y :: z :: Nil => Some(Triple(x, y, z))
      case _ => None
    }

    sealed trait Index

    case object First extends Index

    case object Second extends Index

    case object Third extends Index
  }

  trait Walker[-A, M, +R] { // A is contravariant, R is covariant

    def init: M

    def next(element: A, previous: M): M

    def stop(last: M): R

    // Exercise 5: Implement contramap
    def contramap[B](f: B => A): Walker[B, M, R] = new Walker[B, M, R] {
      def init: M = Walker.this.init

      def next(element: B, previous: M): M =
        Walker.this.next(f(element), previous)

      def stop(last: M): R = Walker.this.stop(last)
    }
  }

  trait Collection[+A] {
    //    def walk(walker: Walker[A, M, R]): R

    //    def map(f: A => B): Collection[B] = ??? // exercise 6 : implement

    //    def flatMap(f: A => Collection[B]) : Collection[B] = ??? // HomeWork 2 : implement
  }

  object Collection {
    def apply[A](seq: A*): Collection[A] = ??? // Homework 1: implement
  }

}

object Subkinding {

  trait Animal

  case class Dog() extends Animal {
    def woof: String = "woof"
  }

  case class Cat() extends Animal {
    def meow: String = "meow"
  }

  type >:>[+A, -B] = <:<[B, A]
  type ???[A, B]   = DummyImplicit

  // sub or super 1
  implicitly[{
      type T[+_]
    } ??? {
      type T[_]
    }
  ]

  // sub or super 2
  implicitly[{
      type T[_]
    } ??? {
      type T[-_]
    }
  ]

  // sub or super 3
  implicitly[{
      type T[_, _]
    } ??? {
      type T[-_, +_]
    }
  ]

  // sub or super 4
  implicitly[{
      type T[_[_]]
    } ??? {
      type T[_]
    }
  ]

  // sub or super 5
  implicitly[{
      type T[_[_]]
    } ??? {
      type T[_[-_]]
    }
  ]

  // sub or super 6
  implicitly[{
      type T[_[+_]]
    } ??? {
      type T[_[-_]]
    }
  ]

  // sub or super 7
  implicitly[{
      type T[_[_[+_]]]
    } ??? {
      type T[_[_[_]]]
    }
  ]

  // sub or super 8
  implicitly[{
      type T[_ >: Dog <: Animal]
    } ??? {
      type T[_]
    }
  ]

  // sub or super 9
  implicitly[{
      type T[_[_ >: Dog <: Animal]]
    } ??? {
      type T[_[_]]
    }
  ]

  // sub or super 10
  implicitly[{
      type T[_[x] <: Iterable[_]]
    } ??? {
      type T[_[_]]
    }
  ]

  type Matrix[+A] = Vector[Vector[A]]
  val m: Matrix[Int] = Vector(Vector(1, 2), Vector(3, 4))
  val u: Matrix[Any] = m :+ Vector("aaa", "bbb")

  type Printers[-A] = List[A => String]

  val printers: Printers[Animal] = List({
    case _: Cat => "I'm a cat"
    case _: Dog => "I'm a dog"
  },
    x => x.toString
  )
  printers.map(_.apply(Dog()))
  printers.map(_.apply(Cat()))

  val dogPrinters: Seq[Dog => String] = printers :+ ((dog: Dog) => dog.woof)
  dogPrinters.map(_.apply(Dog()))
}
