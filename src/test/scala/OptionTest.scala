import java.util.NoSuchElementException

import org.scalatest.FunSuite

class OptionTest extends FunSuite {

  //region Creating optionals
  test("Option.some") {
    val option = Some("something")
    assert(option.get === "something")
  }

  test("Option.none") {
    val option = None
    assertThrows[NoSuchElementException] {
      option.get
    }
  }

  test("Option.WithValue") {
    val option = Option("something")
    assert(option.get === "something")
  }

  test("Option.WithNullValue") {
    val option = Option(null)
    assertThrows[NoSuchElementException] {
      option.get
    }
  }

  test("Option.empty") {
    val option = Option.empty
    assertThrows[NoSuchElementException] {
      option.get
    }
  }
  //endregion

  // region if value is present
  test("Option.isEmpty") {
    val option = Option.empty
    assert(option.isEmpty)
  }

  test("Option.nonEmpty") {
    val option = Option("something")
    assert(option.nonEmpty)
  }

  test("Option.isDefined") {
    val option = Option("something")
    assert(option.isDefined)
  }
  test("Option.exist") {
    val option = Option("something")
    assert(option.exists(s => s.nonEmpty))
  }
  //endregion

  // region Default values
  test("Option.getOrElse") {
    val option = None
    assert(option.getOrElse("none") === "none")
  }

  test("Option.orElse") {
    val option = Option(null)
    assert(option.orElse(Option("none")).get === "none")
  }

  test("Option.orNull") {
    val option = Option.empty
    assert(option.orNull == null)
  }
  //endregion

  // region Filter optional

  test("Option.filter") {
    val option = Option("something")
    assert(option.filter(v => v.length > 5).get === "something")
  }

  test("Option.filterNot") {
    val option = Option("something")
    assertThrows[NoSuchElementException] {
      option.filterNot(v => v.length > 5).get
    }
  }
  //endregion

  // region Map optional
  test("Option.map") {
    val option = Option("something")
    val intOption = option.map(v => v.length)
    assert(intOption.get === 9)
  }

  test("Option.flatMap") {
    val option = Option("something")
    val optString = option.flatMap(v => Option(v))
    assert(optString.get === "something")
  }

  //endregion

  // region Pattern Matching

  test("Option.match") {
    val option = Option("something")
    option match {
      case Some(value) => succeed
      case None => fail()
    }
  }

  test("Option.matchWithValues") {
    val option = Option(null)
    val result = option match {
      case Some(value) => value
      case None => "none"
    }
    assert(result === "none")
  }
  //endregion

  test("Option.fold") {
    val option = Option("something")
    val result = option.fold("none")(v => v.toUpperCase)
    assert(result === "SOMETHING")
  }

  test("Option.for") {
    val option = Option("something")

    val result = for {
      opt <- option
      upperValue <- Option(opt.toUpperCase)
    } yield upperValue
    assert(result.get === "SOMETHING")
  }
}