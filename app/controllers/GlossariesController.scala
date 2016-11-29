package controllers

import javax.inject._

import org.scalarules.engine.Fact
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc._
import services.GlossariesService

import scala.language.postfixOps
import scala.reflect.ClassTag

//scalastyle:off public.methods.have.type

class GlossariesController @Inject() (glossariesService: GlossariesService) extends Controller {

  private val glossariesToExpose: Map[String, Map[String, Fact[Any]]] = glossariesService.glossaries.mapValues( g => g.facts )

  def list = Action(
    Ok(Json.toJson(glossariesToExpose))
  )

  def mergedGlossaries = Action(
    Ok(Json.toJson(glossariesService.mergedGlossaries))
  )

  def byId(id: String) = Action(
    glossariesToExpose.get(id).map( Json.toJson(_) ).map( Ok(_) ).getOrElse( NotFound("Element not found") )
  )

  // TODO : Move to something belonging to Fact, but which is within the implicit scope
  implicit def factWrites[T : ClassTag]: Writes[Fact[T]] = (
    (JsPath \ "name").write[String] and
    (JsPath \ "description").write[String] and
    (JsPath \ "valueClass").write[String]
  )( f => (f.name, f.description, f.valueType) )

}
