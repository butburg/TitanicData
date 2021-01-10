import org.scalatest.FunSuite
import titanic._

class UtilsTest extends FunSuite {

  // load datsets                    
  val train = Utils.loadDataCSV("train.csv")
  val test = Utils.loadDataCSV("test.csv")
  val all = train ++ test

  test("Test size of the datesets") {

    assert(train.size === 891)
    assert(test.size === 418)
  }

  test("Count missing values test") {
    
    val attList = List("passengerID", "pclass", "survived", "name", "sex", "age", "sibsp", "parch",
      "ticket", "fare", "cabin", "embarked")

    val train_mv = Utils.countAllMissingValues(train, attList)
    val test_mv = Utils.countAllMissingValues(test, attList)
    assert(train_mv("cabin") == 687 && train_mv("age") == 177 && train_mv("embarked") == 2)
    assert(test_mv("cabin") == 327 && test_mv("age") == 86 && test_mv("fare") == 1)
  }
}