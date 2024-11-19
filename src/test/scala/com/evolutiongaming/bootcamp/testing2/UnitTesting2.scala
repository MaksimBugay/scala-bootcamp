package com.evolutiongaming.bootcamp.testing2

import cats.Monad
import cats.data.State
import org.scalatest.funsuite.{AnyFunSuite, AsyncFunSuite}

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{ExecutionContext, Future}

// *Exercise 13*
//
// What are main problems of asynchronous testing? Try to name or guess them
// without reading further.
//
// One of the biggest is that we cannot really say if the test did not complete
// because of the bug or because it is still working.
//
// Modify Exercise 12 code so the test fails with a timeout by forgetting
// to complete the Promise (a bug), or just doing a long work (by calling
// `Thread.sleep(...)` in the code).
//
// Hint: never use `Thread.sleep` in your production code or test unless you
// are really sure what are you doing. We can discuss why later if we have
// the time.

// *Exercise 14*
//
// Warning: We did not introduce you to some of the
// concepts used below, but I really wanted you to show the trick, so you
// can have something to look forward to. It is fine if you do not understand
// what is happening below. Feel free to ask questions though.
//
// How can we avoid the issues of asynchronous execution? We can abstract
// over the way we execute!
//
object Exercise14 {

  // Remember what other structure allowed to do `map` and `flatMap`? It is
  // `Option`! What if we could replace `Future` with `Option` in our code
  // and make it synchronous magically.
  //
  // Remember Scala allows you to do pass type parameters to function in
  // classes like this?
  def function1[T](list: List[T]): Option[T] = list.headOption

  // And then call the function like this?
  val result: Option[Int] = function1(List(1, 2, 3))

  // We will use the same trick now and pass `Option` instead of `Future`
  // to the code.
  //
  // There is one catch. You have to use `T[_]` instead of `T` if you pass
  // a higher kinded type (see previous lecture).
  //
  case class Player(id: String, name: String, email: String, score: Int)

  trait PlayerRepository[F[_]] {
    def byId(id: String): F[Option[Player]]

    def all: F[List[Player]]

    def update(player: Player): F[Unit]

    def delete(id: String): F[Unit]
  }

  trait Logging[F[_]] {
    def info(message: String): F[Unit]
  }

  trait PlayerService[F[_]] {

    /** Deletes all the players with score lower than minimum.
     *
     * @param miniumumScore the minimum score the player stays with.
     */
    def deleteWorst(minimumScore: Int): F[Unit]

    /** Adds bonus points to score to all existing players
     *
     * @param bonus the bonus points to add to the players.
     */
    def celebrate(bonus: Int): F[Unit]

  }

  class FuturePlayerRepository(initialPlayers: List[Player])(implicit ec: ExecutionContext)
    extends PlayerRepository[Future] {

    private val storage: AtomicReference[List[Player]] = new AtomicReference(initialPlayers)

    override def byId(id: String): Future[Option[Player]] = Future {
      storage.get().find(_.id == id)
    }

    override def all: Future[List[Player]] = Future {
      storage.get()
    }

    override def update(player: Player): Future[Unit] = Future {
      storage.updateAndGet(_.map {
        case p if p.id == player.id => player
        case p => p
      })
      ()
    }

    override def delete(id: String): Future[Unit] = Future {
      storage.updateAndGet(_.filterNot(_.id == id))
      ()
    }
  }

  class FutureLogging(implicit ec: ExecutionContext) extends Logging[Future] {
    @volatile var messages: List[String] = Nil

    override def info(message: String): Future[Unit] = Future {
      synchronized {
        messages = message :: messages
      }
    }

    def getMessages: List[String] = synchronized {
      messages
    }
  }

  import cats.syntax.all._

  object PlayerService {
    def apply[F[_] : Monad](
                             repository: PlayerRepository[F],
                             logging: Logging[F]
                           ): PlayerService[F] = new PlayerService[F] {

      override def deleteWorst(minimumScore: Int): F[Unit] = for {
        all <- repository.all
        lowerThan = all.filter(_.score < minimumScore)
        _ <- lowerThan.traverse { player =>
          repository.delete(player.id) *>
            logging.info(s"Deleted player with id: ${player.id}")
        }
      } yield ()

      override def celebrate(bonus: Int): F[Unit] = for {
        all <- repository.all
        _ <- all.traverse { player =>
          repository.update(player.copy(score = player.score + bonus)) *>
            logging.info(s"Updated player ${player.id} with bonus $bonus")
        }
      } yield ()
    }
  }

}

// Now let's first copy-paste the code form Exercise 12 and replace all
// `Future` calls by `F` to prove it actually works.
//
// sbt:scala-bootcamp> testOnly *testing2.Exercise14FutureSpec
//
class Exercise14FutureSpec extends AsyncFunSuite {

  import Exercise14._

  class Fixture {
    val repository = new FuturePlayerRepository(
      List(
        Player("1", "Ivan", "test0@mail.com", 3),
        Player("2", "Igor", "test1@mail.com", 5),
        Player("3", "Oleg", "test2@mail.com", 7)
      )
    )
    val logging = new FutureLogging()
    val service: PlayerService[Future] = PlayerService[Future](repository, logging)
  }

  test("PlayerService.deleteWorst works correctly") {

    // construct fixture
    val fixture: Fixture = new Fixture

    import fixture._

    // perform the test
    service.deleteWorst(5).flatMap { _ =>
      // validate the repository state
      repository.all.map { players =>
        assert(players == List(
          Player("2", "Igor", "test1@mail.com", 5),
          Player("3", "Oleg", "test2@mail.com", 7)
        ))
      }
    }.flatMap { _ =>
      // validate logging state
      assert(logging.messages.contains("Deleted player with id: 1"))
    }
  }

  ignore("PlayerService.celebrate works correctly") {

    // construct fixture
    val repository = ???
    val logging = ???
    val service = PlayerService[Future](repository, logging)

    // perform the test
    service.celebrate(???) map { _ =>
      // validate the results
      assert(???)
    }

  }

}

// Now let's copy-paste the code form above and replace all
// `Future` calls by `Option` to enjoy rock stable tests.
//
// sbt:scala-bootcamp> testOnly *testing2.Exercise14OptionSpec
//
// Bonus task: try to break the tests with `Thread.sleep(...)` call
// as above.
//
class Exercise14OptionSpec extends AnyFunSuite {

  import Exercise14._

  test("PlayerService.deleteWorst works correctly") {

    // construct fixture
    val repository = ???
    val logging = ???
    val service = PlayerService[Option](repository, logging)

    // perform the test
    service.deleteWorst(???) map { _ =>
      // validate the results
      assert(???)
    }
  }

  test("PlayerService.celebrate works correctly") {

    // construct fixture
    val repository = ???
    val logging = ???
    val service = PlayerService[Option](repository, logging)

    // perform the test
    service.celebrate(???) map { _ =>
      // validate the results
      assert(???)
    }

  }

}
//
// How can we be sure `Future` and `Option` behave the same way?
//
// We know they both have the same methods `map` and `flatMap`, i.e. they are Monads,
// but these could be doing anything, no?
//
// This is a valid question, and you cannot just rely on them behaving similar
// unless proven.
//
// The "behavior" of the similar structures is usually called a law. The laws
// for common structures such as `Monad` or `Traversable` are proven and commonly
// tested using property-based testing libraries such as ScalaCheck.
//
// These libraries allow checking that the specific property stands true not
// for a single parameter value, but for a whole range or domain.
//
// We will not do property testing this time, but you can read more about it here:
// https://www.scalatest.org/user_guide/property_based_testing

// What is the big problem with the code we wrote above? We use `var` and
// `AtomicReference` in our stubs a lot. It is a mutable state. Scala developers
// hate mutable state a lot.
//
// Besides that, we have to go inside of the stub and take data out of it sometimes,
// which might not always be convenient. Can we use our `Option` trick to somehow
// avoid it?
//
// And yes, we can, there is a special type in `cats` library mentioned above.
// It is called `State` and also have the `map` and `flatMap` methods we need.
// Plus it has "state" storage which we can use in our tests!
//
// Hint: `State` is rarely useful outside of testing. There are much better and
// more performant structures for that such as `Ref`. Do not go and refactor all
// your code to `State`.
//
// You can read more about it here:
// https://typelevel.org/cats/datatypes/state.html
//
// Let's use it to simplify our tests a bit.
//
// sbt:scala-bootcamp> testOnly *testing2.Exercise14StateSpec
//
// Bonus task: also store logging statements in `State`.
//



class Exercise14StateSpec extends AnyFunSuite {

  import Exercise14._

  type StateEnv[T] = State[(List[Player], List[String]), T]

  // Repository using StateEnv
  class StatePlayerRepository extends PlayerRepository[StateEnv] {
    override def byId(id: String): StateEnv[Option[Player]] = State { case (players, logs) =>
      ((players, logs), players.find(_.id == id))
    }

    override def all: StateEnv[List[Player]] = State { case (players, logs) =>
      ((players, logs), players)
    }

    override def update(player: Player): StateEnv[Unit] = State.modify { case (players, logs) =>
      (players.map(p => if (p.id == player.id) player else p), logs)
    }

    override def delete(id: String): StateEnv[Unit] = State.modify { case (players, logs) =>
      (players.filterNot(_.id == id), logs)
    }
  }

  // Logging using StateEnv
  class StateLogging extends Logging[StateEnv] {
    override def info(message: String): StateEnv[Unit] = State.modify { case (players, logs) =>
      (players, logs :+ message)
    }
  }
  test("PlayerService.deleteWorst works correctly with State") {
    // Setup initial state
    val initialPlayers = List(
      Player("1", "Ivan", "test0@mail.com", 3),
      Player("2", "Igor", "test1@mail.com", 5),
      Player("3", "Oleg", "test2@mail.com", 7)
    )
    val initialLogs = List.empty[String]
    val repository = new StatePlayerRepository
    val logging = new StateLogging
    val service: PlayerService[StateEnv] = PlayerService(repository, logging)

    // Run the test
    val program: StateEnv[Unit] = for {
      _ <- service.deleteWorst(5)
      players <- repository.all
      _ <- State.inspect[(List[Player], List[String]), Unit] { _ =>
        assert(players == List(
          Player("2", "Igor", "test1@mail.com", 5),
          Player("3", "Oleg", "test2@mail.com", 7)
        ))
      }
    } yield ()

    // Validate final state
    val ((finalPlayers, finalLogs), _) = program.run((initialPlayers, initialLogs)).value
    assert(finalPlayers == List(
      Player("2", "Igor", "test1@mail.com", 5),
      Player("3", "Oleg", "test2@mail.com", 7)
    ))
    assert(finalLogs.contains("Deleted player with id: 1"))
  }

  ignore("PlayerService.celebrate works correctly") {

    // construct fixture
    val repository = ???
    val logging = ???
    val service = PlayerService[StateEnv](repository, logging)

    // perform the test
    service.celebrate(???) map { _ =>
      // validate the results
      assert(???)
    }
  }
}
