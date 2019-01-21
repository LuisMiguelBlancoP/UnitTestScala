import org.scalatest.FunSuite

import scala.util.{Failure, Success, Try}

class TryTest extends FunSuite {

  def toInt(value: String): Try[Int] = {
    Try(Integer.parseInt(value))
  }

  //region Creating Try
  test("Try.success") {
    val tryVal: Try[String] = Success("fine")
    assert(tryVal.get === "fine")
  }

  test("Try.failure") {
    val tryVal: Try[String] = Failure(new NumberFormatException("fail"))
    assertThrows[NumberFormatException] {
      tryVal.get
    }
  }

  test("Try.withValue") {
    val tryVal: Try[String] = Try("fine")
    assert(tryVal.get === "fine")
  }

  test("Try.withNullValue") {
    val tryVal: Try[String] = Try(null)
    assert(tryVal.get == null)
  }

  //endregion

  test("Try.isSuccess") {
    val tryVal: Try[Int] = toInt("1")
    assert(tryVal.isSuccess)
  }

  test("Try.isFailure") {
    val tryVal: Try[Int] = toInt("wrong")
    assert(tryVal.isFailure)
  }

  // region Default values
  test("Try.getOrElse") {
    val tryVal: Try[Int] = toInt("wrong")
    assert(tryVal.getOrElse(2) === 2)
  }

  test("Try.orElse") {
    val tryVal: Try[Int] = toInt("wrong")
    assert(tryVal.orElse(toInt("2")).get === 2)
  }

  test("Try.recover") {
    val tryVal: Try[Int] = toInt("wrong")
    assert(tryVal.recover {
      case e: NumberFormatException => 1
      case e: ArrayIndexOutOfBoundsException => 2
    }.get === 1)
  }
  //endregion

  test("Try.failed") {
    val tryVal1: Try[Int] = toInt("1")
    val tryVal2: Try[Int] = toInt("wrong")

    val inverted1 = tryVal1.failed
    val inverted2 = tryVal2.failed

    assertThrows[UnsupportedOperationException] {
      inverted1.get
    }
    assert(inverted2.isSuccess)
    assert((inverted2.get match {
      case e: NumberFormatException => 2
      case _ => 1
    }) === 2)
  }

  test("Try.filter") {
    val tryVal: Try[Int] = toInt("3")
    assertResult(2) {
      tryVal.filter(v => v == 1).getOrElse(2)
    }
  }

  //region Mapping Try
  test("Try.map") {
    val tryVal: Try[Int] = toInt("2")
    assertResult(4) {
      tryVal.map(v => v * v).get
    }
  }

  test("Try.flatMap") {
    val tryVal: Try[Int] = toInt("2")
    assertResult(4) {
      tryVal.flatMap(v => toInt((v * v).toString)).get
    }
  }
  //endregion

  // region Pattern Matching

  test("Try.match") {
    val tryVal: Try[Int] = toInt("1")
    tryVal match {
      case Success(value) => succeed
      case Failure(e) => fail()
    }
  }

  test("Try.matchWithValues") {
    val tryVal: Try[Int] = toInt("wrong")
    val result = tryVal match {
      case Success(value) => value
      case Failure(e) => 3
    }
    assert(result === 3)
  }
  //endregion

  test("Try.fold") {
    val tryVal: Try[Int] = toInt("wrong")
    val result = tryVal.fold(_ => 1, v => v * v)
    assert(result === 1)
  }

  test("Try.transform") {
    val tryVal: Try[Int] = toInt("wrong")
    val transformed = tryVal.transform(_ => toInt("2"), _ => toInt("3"))
    assert(transformed.get === 3)
  }

  test("Try.for") {
    val tryVal: Try[Int] = toInt("2")

    val result = for {
      opt <- tryVal
      sqrValue <- Try(opt * opt)
    } yield sqrValue
    assert(result.get == 4)
  }

  test("Try.toOption") {
    val tryVal: Try[Int] = toInt("2")
    val opt = tryVal.toOption
    assert(opt.nonEmpty)
    assertResult(2) {
      opt.get
    }
  }
}
