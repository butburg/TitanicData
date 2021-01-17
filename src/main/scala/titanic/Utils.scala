package titanic

import java.io.PrintWriter

import scala.util.Try


object Utils {

  // Regular Expressions for extracting the information
  val DATA_ACCESS_PATTERN_test = """(\d+),(\d),"(.+)",(male|female),([0-9]*\.[0-9]+|[0-9]+|d*),(\d*),(\d*),(.*),([0-9]*\.[0-9]+|[0-9]+|d*),(.*),(\w*)""".r
  val DATA_ACCESS_PATTERN_train = """(\d+),(\d),(\d),"(.+)",(male|female),([0-9]*\.[0-9]+|[0-9]+|d*),(\d*),(\d*),(.*),([0-9]*\.[0-9]+|[0-9]+|d*),(.*),(\w*)""".r

  // Reading text file
  // Stores the information in a map consisting of a property name (key) and its value
  def loadDataCSV(filename: String): List[Map[String, Any]] = {

    val stream = getClass.getResourceAsStream("/" + filename)
    val src = scala.io.Source.fromInputStream(stream)
    val iter = src.getLines().drop(1) //skip first line (property names)

    val result = (for (row <- iter) yield readData(row)).toList

    src.close
    result.flatMap(_ match { case p: Option[Map[String, Any]] => p })
  }


  // Extracting all information storing it into a Map[String,Any]
  def readData(line: String): Option[Map[String, Any]] = {

    def toInt(key: String, s: String): Option[(String, Int)] = Try(s.toInt).toOption.map((key, _))

    def toFloat(key: String, s: String): Option[(String, Float)] = Try(s.toFloat).toOption.map((key, _))

    def toString(key: String, s: String): Option[(String, String)] =
      if (s.nonEmpty) Some((key, s)) else None

    def createPassengerMap(t1: String, t2: String, t3: String, t4: String, t5: String, t6: String, t7: String,
                           t8: String, t9: String, t10: String, t11: String, t12: String): Option[Map[String, Any]] = {

      val l = List(
        toInt("passengerID", t1),
        toInt("survived", t2),
        toInt("pclass", t3),
        toString("name", t4),
        toString("sex", t5),
        toFloat("age", t6),
        toInt("sibsp", t7),
        toInt("parch", t8),
        toString("ticket", t9),
        toFloat("fare", t10),
        toString("cabin", t11),
        {
          if (t12.length > 0) Some(("embarked", t12(0))) else None
        })
      Some(l.flatMap(_ match { case p: Option[(String, Any)] => p }).toMap)
    }

    val result = line match {
      case DATA_ACCESS_PATTERN_test(t1, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) =>
        createPassengerMap(t1, "-1", t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)

      case DATA_ACCESS_PATTERN_train(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) => {
        createPassengerMap(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
      }

    }
    result
  }

  // Method for printing a passenger in a readable manner
  def printPassenger(p: Map[String, Any]): Unit = {

    println("\n---------------------------------------------------------------------")
    println("passengerID:" + p.getOrElse("passengerID", -1))
    println("survived:" + p.getOrElse("survived", -1))
    println("pclass:" + p.getOrElse("pclass", -1))
    println("name:" + p.getOrElse("name", "-"))
    println("sex:" + p.getOrElse("sex", "-"))
    println("age:" + p.getOrElse("age", -1))
    println("sibsp:" + p.getOrElse("sibsp", -1))
    println("parch:" + p.getOrElse("parch", -1))
    println("ticket:" + p.getOrElse("ticket", "-"))
    println("fare:" + p.getOrElse("fare", -1))
    println("cabin:" + p.getOrElse("cabin", -1))
    println("embarked:" + p.getOrElse("embarked", '-'))
    println("---------------------------------------------------------------------\n")
  }


  /**
   * Returns all missing values by attribute
   *
   * @param passengers list of passengers attribute maps
   * @param attList    A mapping from attributes to missing values count
   * @return
   * PassengerId, Survived, Pclass,  Name,                      Sex,  Age,SibSp,Parch,Ticket,   Fare,Cabin,Embarked
   * 1,           0,        3,      "Braund,  Mr. Owen Harris", male, 22, 1,    0,    A/5 21171,7.25, ,    S
   */
  def countAllMissingValues(passengers: List[Map[String, Any]], attList: List[String]): Map[String, Int] =
    passengers.flatMap(passenger => attList.filter(attribut => !passenger.contains(attribut)))
      .groupBy(identity).mapValues(_.size)


  //TODO here erstes
  //
  /*
  * classList = survival->0, survival->1
  * PriorProbability = 342(sur), 549(nots)
  * attributes = l = List(pclass->(1,2,3),fareclass->(1,2,3,4),ageclass->(1,2,3,4))
  *
  *
  * */

  val classListTitanic = Map("survival" -> 0, "survival" -> 1)

  // without PriorProbability :/
  def naiveBayesTrain(
                       passengers: List[Map[String, Any]],
                       className: String = "survived",
                       classList: List[Int] = List(0, 1),
                       attributeMap: Map[String, List[Any]] = Map("pclass" -> List(1, 2, 3), "sex" -> List("male", "female"))
                     ): Map[Int, Map[String, List[Float]]] = {
    classList
      .map(c => c -> attributeMap
        .flatMap(a => Map(a._1 -> a._2
          .map(value => passengers.filter(map => map(className) == c).count(m => m(a._1) == value).toFloat / passengers.count(m => m(className) == c))
        ))).toMap
  }
  //output like:  Map(0 -> Map(pclass -> List(0.14571948, 0.17668489, 0.6775956), sex -> List(0.852459, 0.14754099)),
  //                  1 -> Map(pclass -> List(0.39766082, 0.25438598, 0.34795323), sex -> List(0.31871346, 0.6812866)))


  // passengers.flatMap(passenger => attList.filter(attribut => !passenger.contains(attribut)))
  //    .groupBy(identity).mapValues(_.size)

  def naiveBayesClassify(passengers: List[Map[String, Any]], classList: List[String]): Float = ???

  /*{
    for (c <- classList) yield
      for (d <- eachGivenAttribute) yield
        pc
    ← pc
    · P(d | c)
    return c where p(c) is max
  }
*/




  //produces sometimes an missing argument list error - can be ignored
  def applyModel[CLASS, ID](model: (Map[String, Any], String) => (ID, CLASS),
                            testdata: Seq[Map[String, Any]], idKey: String): Seq[(ID, CLASS)] = {

    testdata.map(d => model(d, idKey))
  }

  def createSubmitFile[ID, CLASS](filename: String, data: Seq[(ID, CLASS)], header: String): Unit = {
    val pw = new PrintWriter(filename)
    pw.println(header)
    data.foreach(e => pw.println(e._1.toString + "," + e._2.toString))
    pw.close()
  }
} 
