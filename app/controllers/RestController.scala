package controllers

import javax.inject.Inject

import controllers.conversion.{Converter, JsonConversionsProvider}
import org.scalarules.engine.{Context, Fact}
import play.api.libs.json.{JsError, JsSuccess}
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
    * @return A JsonObject containing all the facts available in the context, including the originally provided facts and their values.
    */
  def runAll = Action(parse.json) { request =>
    val (initialContextFragments: List[JsSuccess[Context]], conversionErrors: List[JsError]) =
      Converter.convertToIndividualContext(request.body, glossariesService.mergedGlossaries, jsonConversionMap)

    if(conversionErrors != List.empty)
      BadRequest(JsError.toJson(conversionErrors.reduceLeft(_ ++ _)))
    else {
      val initialContext: Context = initialContextFragments.foldLeft(Map.empty[Fact[Any], Any])((acc, jsSuccess) => acc ++ jsSuccess.get)

      val resultContext: Context = RulesRunner.run(initialContext, derivationsService.derivations)

      Ok(Converter.contextToJson(resultContext, jsonConversionMap))
    }
  }

}

