package services

import javax.inject.{Inject, Singleton}

import org.scalarules.engine.Fact
import org.scalarules.utils.Glossary
import play.api.Configuration

@Singleton
class GlossariesService @Inject()(configuration: Configuration, jarLoaderService: JarLoaderService) {

  val glossaries: Map[String, Glossary] = jarLoaderService.jars.flatMap( jarEntry => {
    jarEntry._2.glossaries.map( g => (g.getClass.getName, g) )
  })

  val mergedGlossaries : Map[String, Fact[Any]] = glossaries.values.foldLeft(Map.empty[String, Fact[Any]])((acc, glossary) => acc ++ glossary.facts)

  def findById(id: String): Option[Glossary] = glossaries.get(id)

}
