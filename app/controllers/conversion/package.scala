package controllers

import org.scalarules.facts.Fact
import play.api.libs.json.{JsObject, JsResult, JsValue}

package object conversion {
  type ConvertToFunc = (Fact[Any], JsValue) => JsResult[Any]
  type ConvertBackFunc = (Fact[Any], Any) => JsObject
}
