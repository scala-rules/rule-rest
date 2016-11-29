package controllers

import javax.inject.{Inject, Singleton}

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc._
import services.{JarLoaderService, JarLoadingResults}

import scala.reflect.ClassTag

// scalastyle:off public.methods.have.type

class JarsController @Inject() (jarLoaderService: JarLoaderService) extends Controller {

  def listConfiguration = Action(
    Ok(Json.toJson(jarLoaderService.jarStatusses))
  )

}
