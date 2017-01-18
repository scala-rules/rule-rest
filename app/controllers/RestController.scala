package controllers

import javax.inject.Inject

import controllers.conversion._
import controllers.conversion.Converter._
import org.scalarules.engine.{Context, FactEngine}
import org.scalarules.facts.Fact
import org.scalarules.service.dsl.BusinessService
import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.mvc.{Action, Controller, Request, Result}
import services.{BusinessServicesService, DerivationsService, GlossariesService, JsonConversionMapsService}

import scala.util.{Failure, Success, Try}

// scalastyle:off public.methods.have.type

class RestController @Inject() (businessServicesService: BusinessServicesService,
                                derivationsService: DerivationsService,
                                glossariesService: GlossariesService,
                                jsonConversionMapsService: JsonConversionMapsService) extends Controller {

  val endpoints: String = businessServicesService.businessServiceNames.map{
    case businessService:String => "/api/run/group/" + businessService
  } match {
    case Nil => "No endpoints available: no BusinessServices have been defined!"
    case list: List[String] => list.reduceLeft((acc, next) => acc + "\n" + next)
  }

  def businessservices = Action(
    Ok(endpoints)
  )

  def runBusinessService(name: String) = Action(parse.json) {
    request =>
      findBusinessService(name) match {
        case f: Failure[(String, BusinessService)] => BadRequest(JsError.toJson(JsError(ValidationError(f.exception.getMessage))))
        case s: Success[(String, BusinessService)] => runBusiness(request, DebugResponseJsObject, s.value._2)
      }
  }

  private def findBusinessService(name: String): Try[(String, BusinessService)] = {
    val matchedBusinessServices = businessServicesService.businessServices.collect{ case (naam, service) if naam == name => (naam, service)}
    matchedBusinessServices match {
      case Nil => Failure(
        new IllegalArgumentException("No BusinessService matched this name, make sure you have used the proper endpoint definition!" ++ businessServicesService.businessServiceNames.toString)
      )
      case head :: tail => tail match {
        case Nil => Success(head)
        case tail: List[(String, BusinessService)] => Failure(
          new IllegalStateException("More than one BusinessService matched this name. Suspected mistake in BusinessService specifications.")
        )
      }
    }
  }

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

  private def runBusiness(request: Request[JsValue], jsonResponseProvider: ResponseJsObject, businessService: BusinessService): Result = {
    val (initialContextFragments: List[JsSuccess[Context]], conversionErrors: List[JsError]) = {
      convertToIndividualContext(request.body, businessService.glossaries.foldLeft(Map.empty[String, Fact[Any]])((acc, glossary) => acc ++ glossary.facts), jsonConversionMap)
    }

    if (conversionErrors != List.empty) BadRequest( processConversionErrors(conversionErrors) )
    else processConvertedContextBS(initialContextFragments, jsonResponseProvider, businessService)
  }

  private def processConversionErrors(conversionErrors: List[JsError]): JsObject = JsError.toJson(conversionErrors.reduceLeft(_ ++ _))

  private def processConvertedContext(initialContextFragments: List[JsSuccess[Context]], jsonResponse: ResponseJsObject): JsObject = {
    val initialContext: Context = initialContextFragments.foldLeft(Map.empty[Fact[Any], Any])((acc, jsSuccess) => acc ++ jsSuccess.get)
    val resultContext: Context = RulesRunner.run(initialContext, derivationsService.topLevelDerivations)

    jsonResponse.toJson(initialContext = initialContext, resultContext = resultContext, jsonConversionMap)
  }

  private def processConvertedContextBS(initialContextFragments: List[JsSuccess[Context]], jsonResponse: ResponseJsObject, businessService: BusinessService): Result = {
    val initialContext: Context = initialContextFragments.foldLeft(Map.empty[Fact[Any], Any])((acc, jsSuccess) => acc ++ jsSuccess.get)
    val resultContext: Try[Context] = businessService.run(initialContext, FactEngine.runNormalDerivations)

    resultContext match {
      case f: Failure[Context] => BadRequest( JsError.toJson(JsError(ValidationError("Attempt at calculation failed due to validation errors: " + f.exception.getMessage))) )
      case s: Success[Context] => Ok(
        jsonResponse.toJson(
          initialContext = initialContext,
          resultContext = s.value.filter(x => businessService.uitvoerFacts.contains(x._1)),
          jsonConversionMap
        )
      )
    }

  }

}
