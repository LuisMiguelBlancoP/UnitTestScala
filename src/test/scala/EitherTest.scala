import org.scalatest.FunSuite

import scala.util.{Failure, Success, Try}

class EitherTest extends FunSuite {


  def toInt(value: String): Either[String, Int] = {
    Try(Integer.parseInt(value)) match {
      case Success(v) => Right(v)
      case Failure(e) => Left("Is not a number")
    }
  }

  //region Creating Either
  test("Either.left") {
    val eitherVal: Either[String, Int] = Left("Is not a number")
    assertResult("Is not a number") {
      eitherVal.merge
    }
  }

  test("Either.right") {
    val eitherVal: Either[String, Int] = Right(1)
    assertResult(1) {
      eitherVal.merge
    }
  }
  //endregion

  test("Either.isRight") {
    val eitherVal: Either[String, Int] = toInt("1")
    assert(eitherVal.isRight)
  }

  test("Either.isLeft") {
    val eitherVal: Either[String, Int] = toInt("wrong")
    assert(eitherVal.isLeft)
  }

  test("Either.contains") {
    val eitherVal: Either[String, Int] = toInt("1")
    assert(eitherVal.contains(1))
  }

  test("Either.exist") {
    val eitherVal: Either[String, Int] = toInt("1")
    assert(eitherVal.exists(v => v == 1))
  }

  test("Either.notExist") {
    val eitherVal: Either[String, Int] = toInt("2")
    assert(!eitherVal.exists(v => v == 1))
  }

  test("Either.forAll") {
    val eitherVal: Either[String, Int] = toInt("1")
    assert(eitherVal.forall(v => v == 1))
  }

  test("Either.forAllLeft") {
    val eitherVal: Either[String, Int] = toInt("wrong")
    assert(eitherVal.forall(v => v == 1))
  }

  // region getting values
  test("Either.getOrElse") {
    val eitherVal: Either[String, Int] = toInt("1")
    assertResult(1) {
      eitherVal.getOrElse(2)
    }
  }

  test("Either.filterOrElse") {
    val eitherVal: Either[String, Int] = toInt("3")
    assertResult(3) {
      eitherVal.filterOrElse(v => v == 1, 2).getOrElse(3)
    }
  }
  //endregion

  //region Mapping Either
  test("Either.map") {
    val eitherVal: Either[String, Int] = toInt("2")
    assertResult(4) {
      eitherVal.map(v => v * v).getOrElse(3)
    }
  }

  test("Either.flatMap") {
    val eitherVal: Either[String, Int] = toInt("2")
    assertResult(4) {
      eitherVal.flatMap(v => Right(v * v)).getOrElse(3)
    }
  }

  test("Either.flatMapLeft") {
    val eitherVal: Either[String, Int] = toInt("wrong")
    assertResult(3) {
      eitherVal.flatMap(v => Right(v * v)).getOrElse(3)
    }
  }
  //endregion

  // region Pattern Matching
  test("Either.match") {
    val eitherVal: Either[String, Int] = toInt("2")
    eitherVal match {
      case Right(v1) => succeed
      case Left(v2) => fail()
    }
  }

  test("Either.matchWithValues") {
    val eitherVal: Either[String, Int] = toInt("wrong")
    assertResult(3) {
      eitherVal match {
        case Right(v1) => v1
        case Left(v2) => 3
      }
    }
  }
  //endregion

  test("Either.fold") {
    val eitherVal: Either[String, Int] = toInt("3")
    assertResult(9) {
      eitherVal.fold(_ => 1, v => v * v)
    }
  }

  test("Either.for") {
    val eitherVal1: Either[String, Int] = toInt("2")
    val eitherVal2: Either[String, Int] = toInt("2")
    assertResult(4) {
      (for {
        val1 <- eitherVal1
        val2 <- eitherVal2
      } yield val1 + val2).getOrElse(1)
    }
  }

  test("Either.swap") {
    val eitherVal1: Either[String, Int] = toInt("1")
    val eitherVal2: Either[String, Int] = toInt("wrong")

    val swapped1 = eitherVal1.swap
    assert(swapped1.isLeft)

    val swapped2 = eitherVal2.swap
    assert(swapped2.isRight)

    assertResult(1) {
      swapped1.merge
    }
    assertResult("Is not a number") {
      swapped2.merge
    }
  }


}
