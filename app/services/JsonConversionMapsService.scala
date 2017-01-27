package services

import javax.inject.{Inject, Singleton}

import controllers.conversion._
import play.api.Configuration
import play.api.libs.json.JsValue

@Singleton
class JsonConversionMapsService @Inject()(configuration: Configuration, jarLoaderService: JarLoaderService) {

  private val jsonConversionMaps: Map[String, JsonConversionsProvider] = jarLoaderService.jars.flatMap(jarEntry => {
    jarEntry._2.jsonConversionsProviders.map(g => (g.getClass.getName, g) )
  })

  val mergedJsonConversionMap: JsonConversionsProvider = new JsonConversionsProvider {
    override def jsonToFactConversions: Map[String, ConvertToFunc] = DefaultJsonConversion.jsonToFactConversions ++ jsonConversionMaps.flatMap{
      case (_, map: JsonConversionsProvider) => map.jsonToFactConversions
    }

    override def userSpecifiedConversionsToJson(factValue: Any): JsValue =
      if (jsonConversionMaps.isEmpty) { DefaultJsonConversion.userSpecifiedConversionsToJson(factValue) }
      else if (jsonConversionMaps.size > 1) throw new IllegalStateException("Only a single instance of JsonConversionsProvider may be provided!")
      else { jsonConversionMaps.toList.head._2.userSpecifiedConversionsToJson(factValue) }

  }

}
