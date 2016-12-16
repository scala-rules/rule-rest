package controllers

import javax.inject.Inject

import controllers.conversion._
import controllers.conversion.Converter._
import org.scalarules.engine.Context
import org.scalarules.facts.Fact
import play.api.libs.json.{JsError, JsObject, JsSuccess, JsValue}
import play.api.mvc.{Action, Controller, Request}
import services.{DerivationsService, GlossariesService, JsonConversionMapsService}

// scalastyle:off public.methods.have.type

class RestController @Inject() (derivationsService: DerivationsService, glossariesService: GlossariesService, jsonConversionMapsService: JsonConversionMapsService) extends Controller {

  /**
    * Provides a REST endpoint for triggering all derivations in the target project. Any fact definitions available in the target project's glossaries
    * can be provided in the JSON request body like so:
    * {
    * "factOfTypeString": "factText",
    * "factOfTypeBedrag": 234,
    * "factOfTypeBigDecimal": 234,
    * "factOfTypePercentage": 234
    * }
    *
    * @return A JsonObject containing either:
    *         - A list of JsErrors, containing complete error information concerning failed conversions from json to context (if multiple errors occur, you receive information on all of them)
    *         - A JsObject containing one JsObject: "facts", which contains the combined information of "input" and "results"
    */
  def runAll = Action(parse.json) { request =>
    run(request, DefaultResponseJsObject)
  }

  /**
    * As #runAll except:
    *
    * @return A JsonObject containing either:
    *         - A list of JsErrors, containing complete error information concerning failed conversions from json to context (if multiple errors occur, you receive information on all of them)
    *         - A JsObject containing two JsObject: "input" and "results", which contains only the information of "results"
    */
  def runAllDebug = Action(parse.json) { request =>
    run(request, DebugResponseJsObject)
  }

  /**
    * As #runAll except:
    *
    * @return A JsonObject containing either:
    *         - A list of JsErrors, containing complete error information concerning failed conversions from json to context (if multiple errors occur, you receive information on all of them)
    *         - A JsObject containing one JsObject: "results", which contains only the information of "results"
    */
  def runAllResultsOnly = Action(parse.json) { request =>
    run(request, ResultsOnlyResponseJsObject)
  }


  val jsonConversionMap: JsonConversionsProvider = jsonConversionMapsService.mergedJsonConversionMap

  private def run(request: Request[JsValue], jsonResponseProvider: ResponseJsObject) = {
    val (initialContextFragments: List[JsSuccess[Context]], conversionErrors: List[JsError]) =
      convertToIndividualContext(request.body, glossariesService.mergedGlossaries, jsonConversionMap)

    if (conversionErrors != List.empty) BadRequest( processConversionErrors(conversionErrors) )
    else Ok( processConvertedContext(initialContextFragments, jsonResponseProvider) )
  }

  private def processConversionErrors(conversionErrors: List[JsError]): JsObject = JsError.toJson(conversionErrors.reduceLeft(_ ++ _))

  private def processConvertedContext(initialContextFragments: List[JsSuccess[Context]], jsonResponse: ResponseJsObject): JsObject = {
    val initialContext: Context = initialContextFragments.foldLeft(Map.empty[Fact[Any], Any])((acc, jsSuccess) => acc ++ jsSuccess.get)
    val resultContext: Context = RulesRunner.run(initialContext, derivationsService.topLevelDerivations)

    jsonResponse.toJson(initialContext = initialContext, resultContext = resultContext, jsonConversionMap)
  }

}
