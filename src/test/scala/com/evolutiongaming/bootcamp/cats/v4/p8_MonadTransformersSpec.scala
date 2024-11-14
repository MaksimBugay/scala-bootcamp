package com.evolutiongaming.bootcamp.cats.v4

import cats.data.EitherT
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class p8_MonadTransformersSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {

  final case class User(name: String, giftId: Option[String] = None)
  final case class Gift(name: String)
  final case class GiftCard(id: String)

  val user: User         = User("Bob")
  val gift: Gift         = Gift("Laptop")
  val giftCard: GiftCard = GiftCard("12345")

  trait Repository {
    def fetchUser(): IO[Option[User]]                           = IO.pure(user.some)
    def fetchGiftCard(user: User): IO[Either[String, GiftCard]] = IO.pure(giftCard.asRight)
    def pickGift(giftCard: GiftCard): IO[Gift]                  = IO(gift)
  }

  class GiftFetcher(repository: Repository) {

    def giftT(): EitherT[IO, String, Gift] = {
      for {
        // Fetch the user, returning an error if the user is not found
        user <- EitherT.fromOptionF(repository.fetchUser(), "User not found")

        // Fetch the gift card, returning an error if the gift card is not found
        // We need to map the IO[Either[String, GiftCard]] into EitherT[IO, String, GiftCard]
        giftCard <- EitherT(repository.fetchGiftCard(user))

        // Pick the gift based on the gift card
        gift <- EitherT.liftF(repository.pickGift(giftCard))

      } yield gift
    }
  }

  "GiftFetcher" should {
    "find gift" in {
      val repository  = new Repository {}
      val giftFetcher = new GiftFetcher(repository)

      giftFetcher.giftT().value.asserting(_ shouldBe gift.asRight)
    }

    "not find user" in {
      val repository  = new Repository {
        override def fetchUser(): IO[Option[User]] = IO.pure(None)
      }
      val giftFetcher = new GiftFetcher(repository)

      giftFetcher.giftT().value.asserting(_ shouldBe "User not found".asLeft[Gift])
    }

    "not find gift card" in {
      val repository  = new Repository {
        override def fetchGiftCard(user: User): IO[Either[String, GiftCard]] = IO.pure("Gift card not found".asLeft)
      }
      val giftFetcher = new GiftFetcher(repository)

      giftFetcher.giftT().value.asserting(_ shouldBe "Gift card not found".asLeft[Gift])
    }

    "find gift gift" in {
      val repository  = new Repository {
        override def pickGift(giftCard: GiftCard): IO[Gift] = IO(throw new RuntimeException("Database error"))
      }
      val giftFetcher = new GiftFetcher(repository)

      giftFetcher
        .giftT()
        .value
        .assertThrowsError[RuntimeException](
          _.getMessage shouldBe "Database error"
        )
    }
  }
}
