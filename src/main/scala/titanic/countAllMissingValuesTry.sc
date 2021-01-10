import titanic.Utils

// load datsets
val train = Utils.loadDataCSV("train.csv")
val test = Utils.loadDataCSV("test.csv")
val all = train ++ test

val attList = List("passengerID", "pclass", "survived", "name", "sex", "age", "sibsp", "parch",
  "ticket", "fare", "cabin", "embarked")

def countAllMissingValues(passengers: List[Map[String, Any]], attList: List[String]): Map[String, Int] = {
  passengers.flatMap(passenger => attList.filter(attribut => !passenger.contains(attribut)))
    .groupBy(identity).mapValues(_.size)
}
val train_mv = countAllMissingValues(train, attList)
/*
.withFilter(attribute => passAttr._1.toLowerCase == attribute.toLowerCase &&
            passAttr._2 == null)
          .map(passAttr => (passAttr, 1))
      )).toMap
val train_mv = countAllMissingValues(train, attList)
val test_mv = countAllMissingValues(test, attList)
train_mv("cabin") //== 687
train_mv("age") //== 177
train_mv("embarked") //== 2
test_mv("cabin") //== 327
test_mv("age") //== 86
test_mv("fare") //== 1
*/