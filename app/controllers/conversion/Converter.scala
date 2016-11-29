package controllers.conversion

import controllers.conversion.ImplicitConversions.contextReads._
import controllers.conversion.ImplicitConversions.contextWrites._
import org.scalarules.engine._
import play.api.data.validation.ValidationError
import play.api.libs.json._

object Converter {
  /**
    * Converts the JSON input to facts using the facts provided in the factMap.
    * Any JsValues that are not JsObject will be added to the Errors.
    * Any facts not in the factMap will also be added to the Errors.
    * Any facts whose jsvalue is not of a supported conversion type will also added to the Errors.
    * Returns: a tuple containing:
    *   - a list of JsSuccess containing a context containing a single fact and its value
    *   - a list of JsError containing all errors
    *
    * @param inputJsValue
    * @param factMap A map from a fact's stringname to the corresponding fact.
    * @param jsonConversionMap A map containing a function from Fact[Type] and JsValue to an instance of the correct Type
    * @return (List[JsSuccess], List[JsError])
    */
  def convertToIndividualContext(inputJsValue: JsValue, factMap: Map[String, Fact[Any]], jsonConversionMap: JsonConversionsProvider): (List[JsSuccess[Context]], List[JsError]) = {
    val jsResults: List[JsResult[Context]] = inputJsValue match {
      case c: JsObject => reads(c, factMap, jsonConversionMap)
      case o: Any => List(JsError(ValidationError("No JsValues other than JsObject are allowed!", inputJsValue)))
    }

    val successes : List[JsSuccess[Context]] = jsResults.collect{ case success: JsSuccess[Context] => success }
    val errors: List[JsError] = jsResults.collect{ case error: JsError => error }

    (successes, errors)
  }

  def contextToJson(context: Context, jsonConversionMap: JsonConversionsProvider): JsValue = writes(context, jsonConversionMap)
}
