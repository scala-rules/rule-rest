package controllers

import javax.inject.Inject

import controllers.conversion.{Converter, JsonConversionsProvider}
import org.scalarules.engine.{Context, Fact}
import play.api.libs.json.{JsError, JsObject, JsSuccess}
import play.api.mvc.{Action, Controller}
import services.{DerivationsService, GlossariesService, JsonConversionMapsService}

// scalastyle:off public.methods.have.type

class RestController @Inject() (derivationsService: DerivationsService, glossariesService: GlossariesService, jsonConversionMapsService: JsonConversionMapsService) extends Controller {

  val jsonConversionMap: JsonConversionsProvider = jsonConversionMapsService.mergedJsonConversionMap

  /**
    * Provides a REST endpoint for triggering all derivations in the target project. Any fact definitions available in the target project's glossaries
    * can be provided in the JSON request body like so:
    * {
    *   "factOfTypeString": "factText",
    *   "factOfTypeBedrag": 234,
    *   "factOfTypeBigDecimal": 234,
    *   "factOfTypePercentage": 234
    * }
    *
    * @return A JsonObject containing either:
    *         - A list of JsErrors, containing complete error information concerning failed conversions from json to context (if multiple errors occur, you receive information on all of them)
    *         - A JsObject containing three JsObjects:
    *               - "inputFacts", which contains all the fact information submitted by the caller
    *               - "outputFacts", which contains all the fact information derived by applying the inputFacts to the rules
    *               - "allFacts", which contains the combined information of "inputFacts" and "outputFacts"
    */
  def runAll = Action(parse.json) { request =>
    val (initialContextFragments: List[JsSuccess[Context]], conversionErrors: List[JsError]) =
      Converter.convertToIndividualContext(request.body, glossariesService.mergedGlossaries, jsonConversionMap)

    if(conversionErrors != List.empty)
      BadRequest(JsError.toJson(conversionErrors.reduceLeft(_ ++ _)))
    else {
      val initialContext: Context = initialContextFragments.foldLeft(Map.empty[Fact[Any], Any])((acc, jsSuccess) => acc ++ jsSuccess.get)
      val resultContext: Context = RulesRunner.run(initialContext, derivationsService.derivations)

      Ok(JsObject(
          Map("inputFacts" -> Converter.contextToJson(initialContext, jsonConversionMap)) ++
          Map("outputFacts" -> Converter.contextToJson(resultContext -- initialContext.keys, jsonConversionMap)) ++
          Map("allFacts" -> Converter.contextToJson(resultContext, jsonConversionMap)))
      )
    }
  }

}
