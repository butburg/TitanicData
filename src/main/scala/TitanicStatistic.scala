import scalafx.application.Platform
import titanic.{Utils, VegasUtils}
import vegas.{AggOps, Axis, Bar, Nominal, Ord, Ordinal, Quantitative, Scale, StackOffset, Vegas}

/**
 * @author Edwin W (HTW) on Jan 2021
 */
object TitanicStatistic extends App {

  // load datasets
  val train = Utils.loadDataCSV("train.csv")
  val test = Utils.loadDataCSV("test.csv")
  val all = train ++ test

  println("Train Dataset:" + train.size + " Elements")
  println("Test Dataset:" + test.size + " Elements")
  println("whole Dataset:" + all.size + " Elements")

  Platform.implicitExit_=(false)
  val chart1 = Vegas("Passengers split by sex").
    withData(train).
    mark(Bar).
    encodeX("sex", Ordinal, axis = Axis(title = "Sex")).
    encodeY("passengerID", Quantitative, AggOps.Count, axis = Axis(title = "Passengers"))

  val passengers = train.size
  val survivedPass = train.count(m => m("survived") == 1)
  val rate = survivedPass.toDouble / passengers
  println("propability of surviving:" + rate)

  val chart2 = Vegas("Passengers classified by survival").
    withData(train).
    mark(Bar).
    addTransform("survival", "datum.survived == 0 ? \"Dead\" : \"Alive\"").
    encodeX("survival", Ordinal, axis = Axis(title = "Survival")).
    encodeY("passengerID", Quantitative, AggOps.Count, axis = Axis(title = "Passengers"))

  val chart3 = Vegas("Survival split by sex").
    withData(train).
    mark(Bar).
    addTransform("survival", "datum.survived == 0 ? \"No\" : \"Yes\"").
    encodeY("passengerID", Quantitative, AggOps.Count, axis = Axis(title = "Passengers")).
    encodeX("sex", Ord).
    encodeColor("survival", Nominal, scale = Scale(rangeNominals = List("#EA98D2", "#659CCA")))

  val chart4 = Vegas("Survival split by sex").
    withData(train).
    mark(Bar).
    addTransform("survival", "datum.survived == 0 ? \"No\" : \"Yes\"").
    encodeY("passengerID", Quantitative, AggOps.Count, axis = Axis(title = "Passengers")).
    encodeX("sex", Ord).
    encodeColor("survival", Nominal, scale = Scale(rangeNominals = List("#EA98D2", "#659CCA"))).
    configMark(stacked = StackOffset.Normalize)

  VegasUtils.showAllInBrowser(List(chart1, chart2, chart3, chart4))
}
