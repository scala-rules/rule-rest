package controllers.conversion

import controllers.conversion.ImplicitConversions._
import org.scalarules.facts.Fact
import org.scalarules.finance.nl._
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsObject, _}

import scala.reflect.runtime.universe._

trait JsonConversionsProvider {
  private def convertFacts(fact: Fact[Any], factValue: Any): JsObject = JsObject(Map(fact.name -> turnFactsIntoJson(factValue)))

  private def turnFactsIntoJson(factValue: Any): JsValue = //scalastyle:ignore cyclomatic.complexity
    try { userSpecifiedConversionsToJson(factValue) }
    catch { case e: Exception => factValue match {
      case x :: xs => JsArray(for { elem <- (x :: xs)} yield turnFactsIntoJson(elem))
      case bedrag: Bedrag => Json.toJson(bedrag)
      case string: String => Json.toJson(string)
      case bool: Boolean => JsBoolean(bool)
      case bool: java.lang.Boolean => JsBoolean(bool)
      case bigDecimal: BigDecimal => Json.toJson(bigDecimal)
      case percentage: Percentage => Json.toJson(percentage)
      case other: Any => throw new IllegalStateException(s"No legal conversion found for $other, with type ${other.getClass} " + e.fillInStackTrace())
    }
    }

  def contextToJsonConversions(fact: Fact[Any], factValue: Any): JsObject = convertFacts(fact, factValue)

  /**
    * override this method in your JsonConversionsProvider to add conversions to Json for types you have implemented,
    * without losing all the predefined conversions (custom-specified takes precendence).
    * Example:
    * override def userSpecifiedConversionsToJson(factValue: Any): JsValue = factValue match {
        case thingOfYourType: YourType => Json.toJson[YourType](thingOfYourType)
      }
    * @param factValue
    * @return
    */
  def userSpecifiedConversionsToJson(factValue: Any): JsValue = factValue match {
    case _ => throw new IllegalStateException("None of the default matches succeeded and no other matches were provided")
  }

  def jsonToFactConversions: Map[String, ConvertToFunc]
}

object DefaultJsonConversion extends JsonConversionsProvider {
  override def jsonToFactConversions: Map[String, ConvertToFunc] = JsonToFactConversionMap.jsonToFactConversionMap

  object JsonToFactConversionMap {
    val jsonToFactConversionMap: Map[String, ConvertToFunc] = Map[String, ConvertToFunc](
      weakTypeOf[List[List[List[Bedrag]]]].toString.replace("scala.", "") -> { bedragLijstLijstLijstFunct(_, _) },
      weakTypeOf[List[List[Bedrag]]].toString.replace("scala.", "") -> { bedragLijstLijstFunct(_, _) },
      weakTypeOf[List[Bedrag]].toString.replace("scala.", "") -> { bedragLijstFunct(_, _) },
      classOf[String].getTypeName -> { stringFunct(_, _) },
      weakTypeOf[String].toString -> { stringFunct(_, _) },
      classOf[Bedrag].getTypeName -> { bedragFunct(_, _) },
      classOf[Percentage].getTypeName -> { percentageFunct(_, _) },
      classOf[BigDecimal].getTypeName -> { bigDecimalFunct(_, _) },
      weakTypeOf[BigDecimal].toString -> { bigDecimalFunct(_, _) },
      classOf[Boolean].getTypeName -> { booleanFunct(_, _) },
      weakTypeOf[Boolean].toString -> { booleanFunct(_, _) }
    )

    private def stringFunct(fact: Fact[Any], factValue: JsValue): JsResult[String] = factValue match {
      case jsString: JsString => JsSuccess(jsString.value)
      case _ => JsError(ValidationError(s"Conversion for String fact ${fact.name} failed, corresponding value was not of expected type JsString"))
    }

    private def bigDecimalFunct(fact: Fact[Any], factValue: JsValue): JsResult[BigDecimal] = factValue match {
      case jsNumber: JsNumber => JsSuccess(jsNumber.value)
      case _ => JsError(ValidationError(s"Conversion for BigDecimal fact ${fact.name} failed, corresponding value was not of expected type JsNumber"))
    }

    private def bedragLijstLijstLijstFunct(fact: Fact[Any], factValue: JsValue): JsResult[List[List[List[Bedrag]]]] = factValue match {
      case jsNumber: JsArray => Json.fromJson[List[List[List[Bedrag]]]](jsNumber)
      case _ => JsError(ValidationError(s"Conversion for Bedrag fact ${fact.name} failed, corresponding value was not of expected type JsNumber"))
    }

    private def bedragLijstLijstFunct(fact: Fact[Any], factValue: JsValue): JsResult[List[List[Bedrag]]] = factValue match {
      case jsNumber: JsArray => Json.fromJson[List[List[Bedrag]]](jsNumber)
      case _ => JsError(ValidationError(s"Conversion for Bedrag fact ${fact.name} failed, corresponding value was not of expected type JsNumber"))
    }

    private def bedragLijstFunct(fact: Fact[Any], factValue: JsValue): JsResult[List[Bedrag]] = factValue match {
      case jsNumber: JsArray => Json.fromJson[List[Bedrag]](jsNumber)
      case _ => JsError(ValidationError(s"Conversion for Bedrag fact ${fact.name} failed, corresponding value was not of expected type JsNumber"))
    }

    private def bedragFunct(fact: Fact[Any], factValue: JsValue): JsResult[Bedrag] = factValue match {
      case jsNumber: JsNumber => Json.fromJson[Bedrag](jsNumber)
      case _ => JsError(ValidationError(s"Conversion for Bedrag fact ${fact.name} failed, corresponding value was not of expected type JsNumber"))
    }

    private def percentageFunct(fact: Fact[Any], factValue: JsValue): JsResult[Percentage] = factValue match {
      case jsNumber: JsNumber => Json.fromJson[Percentage](jsNumber)
      case _ => JsError(ValidationError(s"Conversion for Percentage fact ${fact.name} failed, corresponding value was not of expected type JsNumber"))
    }

    private def booleanFunct(fact: Fact[Any], factValue: JsValue): JsResult[Boolean] = factValue match {
      case jsBoolean: JsBoolean => JsSuccess(jsBoolean.value)
      case _ => JsError(ValidationError(s"Conversion for String fact ${fact.name} failed, corresponding value was not of expected type JsBoolean"))
    }

  }

}

