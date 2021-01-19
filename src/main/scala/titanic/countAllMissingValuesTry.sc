import titanic.Utils

import scala.collection.mutable

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

//val attributeMap = Map("pclass" -> List(1, 2, 3), "sex" -> List("male", "female"))
//val classList = List(0, 1)
val wantedAttributes = List("pclass", "sex")
val className = "survived"

def getClassValues(l: List[Map[String, Any]], className: String): List[Any] =
  l.flatMap(map => map.filter(att => att._1 == className)).distinct.map(_._2)

def getAttrsAndValues(l: List[Map[String, Any]], wantedAttributes: List[String]): Map[String, List[Any]] =
  wantedAttributes.map(attr => attr -> getClassValues(l, attr)).toMap
/*
val trainingResult =
  getClassValues(passengers, "survived")
    .map(c => c -> getAttrsAndValues(passengers, wantedAttributes) //<- should be full list: each possible!!!
      .flatMap(a => Map(a._1 -> a._2
        .map(value => passengers.filter(map => map(className) == c).count(m => m(a._1) == value).toFloat / passengers.count(m => m(className) == c))
      ))).toMap
add val priorProb = passengers.count(m => m(className) == c).toFloat / passengers.size somewhere
*/
val trainresultManuell: Map[Int, Map[String, Map[Any, Double]]] = Map(
  0 -> Map("pclass" -> Map(1 -> 0.6775956, 2 -> 0.14571948, 3 -> 0.17668489),
    "sex" -> Map("male" -> 0.852459, "female" -> 0.14754099)),
  1 -> Map("pclass" -> Map(1 -> 0.34795323, 2 -> 00.39766082, 3 -> 0.25438598),
    "sex" -> Map("male" -> 0.31871346, "female" -> 0.6812866)))

getClassValues(passengers, "survived")
getAttrsAndValues(passengers, wantedAttributes)


val trainresult: Map[Any, Map[String, Map[Any, Float]]] = getClassValues(passengers, "survived")
  .map(c =>
    c -> getAttrsAndValues(passengers, wantedAttributes).flatMap(
      a => Map(a._1 -> a._2
        .flatMap(value => Map(value -> passengers.filter(map => map(className) == c).count(m => m(a._1) == value).toFloat / passengers.count(m => m(className) == c))).toMap))
  ).toMap




val isSurvived =
  getClassValues(passengers, "survived")
    .map(c => c -> getAttrsAndValues(passengers, wantedAttributes) //<- should be only the one you will count on, so pclass, ageclass, fareclass, embarked
      .flatMap(a => Map(a._1 -> a._2
        .map(value => passengers.filter(map => map(className) == c).count(m => m(a._1) == value).toFloat / passengers.count(m => m(className) == c))
      ))).toMap

/*
=>Map(0 -> Map( pclass -> List(0.6775956, 0.14571948, 0.17668489),
                sex -> List(0.852459, 0.14754099)),
      1 -> Map( pclass -> List(0.34795323, 0.39766082, 0.25438598),
                sex -> List(0.31871346, 0.6812866)))
 */


val isSurvived = test
  .map(passenger => {
    //println("AttributValues" + trainresult.map(c => (c._1, c._2.flatMap(attribut => passenger.filter(tupel => tupel._1 == attribut._1)))))
    val pc: Map[Any, Float] =
      trainresult
        .map(c => (c._1, c._2.flatMap(trainResAttribute => passenger.filter(tuple => tuple._1 == trainResAttribute._1))))
        .map(c => (c._1, c._2
          .map(att =>
            trainresult
              .filter(_._1 == c._1)
              .map(c => (c._1, c._2
                .flatMap(attribute => attribute._2.filter(value => value._1 == att._2)).values.head)
              ).values
          ).foldLeft(passengers.count(m => m(className) == c).toFloat / passengers.size)((x, y) => x * y.head))
        )

    val survived = pc.maxBy(_._2)
    println("survive?" + survived)
    //trick 17
    val newPassenger: mutable.Map[String, Any] = mutable.Map(passenger.toSeq: _*)
    newPassenger.update("survived", survived._1)
    newPassenger
  })


/*
val isSurvived = {
  var newData = List[collection.Map[String, Any]]()
  //für jeden passagier
  for (passenger <- test) {
    println("START")
    val res = mutable.Map[Any, Float]()
    for (c <- trainresult) {
      val priorProb = passengers.count(m => m(className) == c._1).toFloat / passengers.size
      println(priorProb)
      //für jedes trainresult attribut:
      var pc = priorProb * 100 //<--- var not good!!!

      for (attribut <- c._2) {
        //println(passenger)
        //println(c)
        println(attribut)
        //wenn trainresult attribut gleich passagier attribut ist
        println(passenger.filter(tupel => tupel._1 == attribut._1))
        //hole den P wert aus trainresult entsprechend value des passagier attributes
        //println(passenger.filter(tupel => tupel._1 == attribut._1).values.head)
        val why = passenger.filter(tupel => tupel._1 == attribut._1).values.head
        println(attribut._2)

        //bsp ist ageclass 2 dann holde den zweiten value
        //println(attribut._2.filter(tupel => tupel._1 == why))
        val p = attribut._2.filter(tupel => tupel._1 == why).values.head
        //multipliziere den value mit dem nächsten, anfangswert ist prioProbability (reduceLeft)
        println("pc:" + pc + "*" + p.toFloat + " * 100=" + pc * p.toFloat * 100)
        pc = pc * p.toFloat * 100
        //ordne den wert der c klasse zu
        res.update(c._1, pc)
      }
      println("PC: " + pc)
    }
    //hole den größten wert und die entsprechende c klasse => ergebnis
    println(res)
    println("survive?" + res.maxBy(_._2))
    val newPassenger: mutable.Map[String, Any] = mutable.Map(passenger.toSeq: _*)
    newPassenger.update("survived", res.maxBy(_._2)._1)
    println(newPassenger)

    newData = newPassenger :: newData
  }
  println("NewData:" + newData)
  newData
}
/*

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