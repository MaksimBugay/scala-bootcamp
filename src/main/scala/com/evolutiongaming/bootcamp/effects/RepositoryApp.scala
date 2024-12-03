package com.evolutiongaming.bootcamp.effects

import cats.Monad
import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.collection.mutable

object RepositoryApp extends IOApp {
  case class User(id: String)

  // TODO: implement in memory dao
  // doesn't have to be thread safe
  class UserDao[F[_] : Monad] {
    private val users = mutable.ListBuffer.empty[User]

    def getAllUsers: F[List[User]] = Monad[F].pure(users.toList)

    def addUser(user: User): F[Unit] = Monad[F].pure {
      users += user
    }
  }

  def program[F[_] : Monad](users: UserDao[F]) = {
    for {
      beforeUsers <- users.getAllUsers
      _ = println(beforeUsers)
      _ <- List(User("Vera"), User("Katya")).traverse(users.addUser)
      updatedUsers <- users.getAllUsers
      _ = println(updatedUsers)
    } yield ()
  }

  //program[IO](new UserDao[IO]).unsafeRunSync()

  // wait for future
  Thread.sleep(500)

  private val ios = (1 to 10).map(x => IO.delay(println(x))).toList

  override def run(args: List[String]): IO[ExitCode] = {
    IO.parSequenceN(10)(ios).unsafeRunSync()
    program[IO](new UserDao[IO]).map(_ => ExitCode.Success)
  }
}

//class IO[A] (unsafeRunSync: => A)
