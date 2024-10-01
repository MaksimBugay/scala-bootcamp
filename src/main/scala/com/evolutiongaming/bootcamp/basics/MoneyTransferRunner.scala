package com.evolutiongaming.bootcamp.basics

import com.evolutiongaming.bootcamp.basics.ControlStructures.{Amount, ErrorMessage, UserId, UserService, makeTransfer}

object MoneyTransferRunner extends App {
  val userService: UserService = new UserService {
    override def validateUserName(name: String): Either[ErrorMessage, Unit] = {
      if (name.nonEmpty) Right(()) else Left("Invalid username")
    }

    override def findUserId(name: String): Either[ErrorMessage, UserId] = {
      if (name == "Alice") Right("user1") else if (name == "Bob") Right("user2") else Left("User not found")
    }

    override def validateAmount(amount: Amount): Either[ErrorMessage, Unit] = {
      if (amount > 0) Right(()) else Left("Invalid amount")
    }

    override def findBalance(userId: UserId): Either[ErrorMessage, Amount] = {
      userId match {
        case "user1" => Right(BigDecimal(100))
        case "user2" => Right(BigDecimal(50))
        case _ => Left("Balance not found")
      }
    }

    override def updateAccount(userId: UserId, previousBalance: Amount, delta: Amount): Either[ErrorMessage, Amount] = {
      val newBalance = previousBalance + delta
      if (newBalance < 0) Left("Insufficient balance") else Right(newBalance)
    }
  }
  // Example of successful transfer
  val result = makeTransfer(userService, "Alice", "Bob", BigDecimal(20))
  result match {
    case Right((fromBalance, toBalance)) =>
      println(s"Transfer successful! Alice's new balance: $fromBalance, Bob's new balance: $toBalance")
    case Left(error) =>
      println(s"Transfer failed: ${error}")
  }
}
