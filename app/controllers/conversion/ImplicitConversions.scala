package controllers.conversion

import org.scalarules.engine.Fact
import org.scalarules.engine.Context
import org.scalarules.finance.nl._
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.annotation.tailrec

object ImplicitConversions {

  implicit object contextWrites {
    /**
      * Writes a context to a JsonObject containing all existing facts and their corresponding values,
      * using a conversionMap that provides a function how to unwrap facts from a given type.
      *
      * @param context
      * @param conversionMap
      * @return a Json Object containing all existing facts and their corresponding values
      */
    def writes(context: Context, conversionMap: JsonConversionsProvider): JsValue = {
      context.map{ case (fact:Fact[Any], factValue: Any) =>
        conversionMap.contextToJsonConversions.get(factValue.getClass) match {
          case function: Some[ConvertBackFunc] => function.get(fact, factValue)
          case None => throw new IllegalStateException(s"Unable to find suitable toJson conversion for Fact with name ${fact.name} with " +
                                                        s"valuetype ${factValue.getClass.getTypeName} in factConversionMap")
        }
      }.reduceLeft(_ ++ _)
    }
  }

  implicit object contextReads {
    def reads(jsObject: JsObject, possibleFacts: Map[String, Fact[Any]], conversionMap: JsonConversionsProvider): List[JsResult[Context]] = {
      val prospectiveFacts: List[(String, JsValue)] = jsObject.fields.toList

      def jsResultFactToContext(fact: Fact[Any], jsResult: JsResult[Any]): JsResult[Context] = jsResult match {
        case success: JsSuccess[Any] => JsSuccess(Map(fact -> success.get))
        case error: JsError => error
      }

      @tailrec
      def inner(list: List[(String, JsValue)], resultList: List[JsResult[Context]]): List[JsResult[Context]] = list match {
        case Nil => resultList
        case ((factName: String, factValue: JsValue) :: tail) => possibleFacts.get(factName) match {
          case Some(fact) => conversionMap.jsonToFactConversions.get(fact.valueType) match {
            case function: Some[ConvertToFunc] => inner(tail, jsResultFactToContext(fact, function.get(fact, factValue)) :: resultList)
            case None => inner(tail, JsError(ValidationError(s"Unable to find suitable fromJson conversion for Fact with name $factName with type ${fact.valueType} in factConversionMap",
                                factValue)) :: resultList)
          }
          case None => inner(tail, JsError(ValidationError(s"Unable to find Fact with name $factName in the Glossary", factValue)) :: resultList)
        }
        case _ => JsError(ValidationError("Something weird has happened during conversion")) :: resultList
      }
      inner(prospectiveFacts, List.empty)
    }
  }

  implicit object bedragReads extends Reads[Bedrag] {
    def reads(jsValue: JsValue): JsResult[Bedrag] = jsValue match {
      case jsNumber: JsNumber => JsSuccess(jsNumber.value.euro)
      case other: Any => JsError(ValidationError("error.invalid.bedrag", other))
    }
  }

  implicit object bedragWrites extends Writes[Bedrag] {
    def writes(bedrag: Bedrag): JsValue = {
      Json.toJson(bedrag.waarde)
    }
  }

  implicit object percentageReads extends Reads[Percentage] {
    def reads(jsValue: JsValue): JsResult[Percentage] = jsValue match {
      case jsNumber: JsNumber => JsSuccess(jsNumber.value.procent)
      case other: Any => JsError(ValidationError("error.invalid.percentage", other))
    }
  }

  implicit object percentageWrites extends Writes[Percentage] {
    def writes(percentage: Percentage): JsValue = {
      Json.toJson(percentage.percentage)
    }
  }
}
