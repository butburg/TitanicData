import titanic.Utils

// load datsets
val train = Utils.loadDataCSV("train.csv")
val passengers = train
val test = Utils.loadDataCSV("test.csv")
val all = train ++ test
val attList = List("passengerID", "pclass", "survived", "name", "sex", "age", "sibsp", "parch",
  "ticket", "fare", "cabin", "embarked")

/*
* classList = survival->0, survival->1
* PriorProbability = 342(sur), 549(nots)
* attributes = l = List(pclass->(1,2,3),fareclass->(1,2,3,4),ageclass->(1,2,3,4))
*
*passengers: List[Map[String, Any]], classList: Map[String, Int], attributes: Map[String,Int]): Float
* */


val attributeMap = Map("pclass" -> List(1, 2, 3), "sex" -> List("male", "female"))
val classList = List(0, 1)
val className = "survived"

val trainingResult =
  classList
    .map(c => c->attributeMap //<- should be full list: each possible!!!
      .flatMap(a => Map(a._1 -> a._2
        .map(value => passengers.filter(map => map(className) == c).count(m => m(a._1) == value).toFloat / passengers.count(m => m(className) == c))
      ))).toMap



val isSurvived =
  classList
    .map(c => c->attributeMap //<- should be only the one you will count on, so pclass, ageclass, fareclass, embarked
      .flatMap(a => Map(a._1 -> a._2
        .map(value => passengers.filter(map => map(className) == c).count(m => m(a._1) == value).toFloat / passengers.count(m => m(className) == c)).map(_ * (passengers.count(m => m(className) == c).toFloat /passengers.size))
      ))).toMap

/*

val isSurvived = for (c <- classList) {
  val classCount = passengers.count(m => m(className) == c)
  println("ClassCount: " + classCount)
  val priorProb = classCount.toFloat / passengers.size
  println("PriorProb" + priorProb)
  val classList = passengers.filter(map => map(className) == c)
  //should be only the one you will count on, so pclass, ageclass, fareclass, embarked
  for (a <- attributeMap) {
    for (value <- a._2) yield {
      val valueCount = classList.count(m => m(a._1) == value).toFloat / classCount
      println(a._1 + "(" + value + "): " + valueCount)
      println(valueCount.toFloat / classCount)
    }
  }
}


val res = for (c <- classList) {
  val classCount = passengers.count(m => m(className) == c)
  println("ClassCount: " + classCount)
  val priorProb = classCount.toFloat / passengers.size
  println("PriorProb" + priorProb)
  val classList = passengers.filter(map => map(className) == c)
  for (a <- attributeMap) {
    for (value <- a._2) yield {
      val valueCount = classList.count(m => m(a._1) == value).toFloat / classCount
      println(a._1 + "(" + value + "): " + valueCount)
      println(valueCount.toFloat / classCount)
    }
  }
}

val res =
  classList.flatMap(c => Map(c ->{
    val classCount = passengers.count(m => m(className) == c)
    val priorProb = classCount.toFloat / passengers.size
    val classList = passengers.filter(map => map(className) == c)
    attributeMap
      .map(a => Map(a._1 -> a._2
        .map(value => classList.count(m => m(a._1) == value).toFloat / classCount)
      ))
  }))

*/



/*val res =
  classList.map(c => {
    val classCount = passengers.count(m => m(className) == c)
    val priorProb = classCount.toFloat / passengers.size
    val classList = passengers.filter(map => map(className) == c)
    attributeMap
      .map(a => a._2
        .map(value => classList.count(m => m(a._1) == value).toFloat / classCount))
  })*/





passengers.flatMap(pass => pass.filter(_._1 == "survived")).count(_._2 == 1)

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