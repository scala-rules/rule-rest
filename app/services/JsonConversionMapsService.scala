package services

import javax.inject.{Inject, Singleton}

import controllers.conversion._
import play.api.Configuration

@Singleton
class JsonConversionMapsService @Inject()(configuration: Configuration, jarLoaderService: JarLoaderService) {

  private val jsonConversionMaps: Map[String, JsonConversionsProvider] = jarLoaderService.jars.flatMap(jarEntry => {
    jarEntry._2.jsonConversionsProviders.map(g => (g.getClass.getName, g) )
  })

  val mergedJsonConversionMap: JsonConversionsProvider = new JsonConversionsProvider {
    override def jsonToFactConversions: Map[String, ConvertToFunc] = DefaultJsonConversion.jsonToFactConversions ++ jsonConversionMaps.flatMap{
      case (_, map: JsonConversionsProvider) => map.jsonToFactConversions
    }

    override def contextToJsonConversions: Map[Class[_], ConvertBackFunc] = DefaultJsonConversion.contextToJsonConversions ++ jsonConversionMaps.flatMap{
      case (_, map: JsonConversionsProvider) => map.contextToJsonConversions
    }
  }

}
