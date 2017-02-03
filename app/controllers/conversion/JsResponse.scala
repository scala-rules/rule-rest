package controllers.conversion

import controllers.conversion.Converter._
import org.scalarules.engine._
import org.scalarules.facts.Fact
import play.api.libs.json.JsObject

trait ResponseJsObject {
  def toJson(initialContext: Context, uitvoerFacts: List[Fact[Any]], resultContext: Context, jsonConversionMap: JsonConversionsProvider): JsObject
}

object InputsAndOutputsResponseJsObject extends ResponseJsObject {
  override def toJson(initialContext: Context, uitvoerFacts: List[Fact[Any]], resultContext: Context, jsonConversionMap: JsonConversionsProvider): JsObject =
    JsObject(Map(
      "inputs" -> contextToJson(initialContext, jsonConversionMap),
      "results" -> contextToJson(resultContext.filter(factWithValue => uitvoerFacts.contains(factWithValue._1)), jsonConversionMap)))
}

object CompleteResponseJsObject extends ResponseJsObject {
  override def toJson(initialContext: Context, uitvoerFacts: List[Fact[Any]], resultContext: Context, jsonConversionMap: JsonConversionsProvider): JsObject = {
    JsObject(Map(
      "inputs" -> contextToJson(initialContext, jsonConversionMap),
      "intermediateSteps" -> contextToJson(resultContext -- initialContext.keys -- resultContext.filter(factWithValue => (uitvoerFacts.contains(factWithValue._1))).keys, jsonConversionMap),
      "results" -> contextToJson(resultContext.filter(factWithValue => uitvoerFacts.contains(factWithValue._1)), jsonConversionMap)))
  }
}

object OutputsOnlyResponseJsObject extends ResponseJsObject {
  override def toJson(initialContext: Context, uitvoerFacts: List[Fact[Any]], resultContext: Context, jsonConversionMap: JsonConversionsProvider): JsObject =
    JsObject(Map(
      "results" -> contextToJson(resultContext.filter(factWithValue => uitvoerFacts.contains(factWithValue._1)), jsonConversionMap)))
}

object RunAllResponseJsObject extends ResponseJsObject {
  override def toJson(initialContext: Context, uitvoerFacts: List[Fact[Any]], resultContext: Context, jsonConversionMap: JsonConversionsProvider): JsObject =
    JsObject(Map("facts" -> contextToJson(resultContext, jsonConversionMap)))
}

object DebugAllResponseJsObject extends ResponseJsObject {
  override def toJson(initialContext: Context, uitvoerFacts: List[Fact[Any]], resultContext: Context, jsonConversionMap: JsonConversionsProvider): JsObject =
    JsObject(Map(
      "inputs" -> contextToJson(initialContext, jsonConversionMap),
      "results" -> contextToJson(resultContext -- initialContext.keys, jsonConversionMap)))
}

object RunAllResultsOnlyResponseJsObject extends ResponseJsObject {
  override def toJson(initialContext: Context, uitvoerFacts: List[Fact[Any]], resultContext: Context, jsonConversionMap: JsonConversionsProvider): JsObject =
    JsObject(Map("results" -> contextToJson(resultContext -- initialContext.keys, jsonConversionMap)))
}

