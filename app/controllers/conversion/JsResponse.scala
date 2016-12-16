package controllers.conversion

import controllers.conversion.Converter._
import org.scalarules.engine._
import play.api.libs.json.JsObject

trait ResponseJsObject {
  def toJson(initialContext: Context, resultContext: Context, jsonConversionMap: JsonConversionsProvider): JsObject
}

object DebugResponseJsObject extends ResponseJsObject {
  override def toJson(initialContext: Context, resultContext: Context, jsonConversionMap: JsonConversionsProvider): JsObject =
    JsObject(Map(
      "inputs" -> Converter.contextToJson(initialContext, jsonConversionMap),
      "results" -> contextToJson(resultContext -- initialContext.keys, jsonConversionMap)))
}

object DefaultResponseJsObject extends ResponseJsObject {
  override def toJson(initialContext: Context, resultContext: Context, jsonConversionMap: JsonConversionsProvider): JsObject =
    JsObject(Map("facts" -> Converter.contextToJson(resultContext, jsonConversionMap)))
}

object ResultsOnlyResponseJsObject extends ResponseJsObject {
  override def toJson(initialContext: Context, resultContext: Context, jsonConversionMap: JsonConversionsProvider): JsObject =
    JsObject(Map("results" -> Converter.contextToJson(resultContext -- initialContext.keys, jsonConversionMap)))
}
