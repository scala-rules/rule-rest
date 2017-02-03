package services

import javax.inject.{Inject, Singleton}

import org.scalarules.derivations.Derivation
import org.scalarules.dsl.nl.grammar.ElementBerekening
import play.api.{Configuration, Logger}

@Singleton
class DerivationsService @Inject() (configuration: Configuration, jarLoaderService: JarLoaderService) {

  private val elementBerekeningClass = classOf[ElementBerekening[Any, Any]]

  val topLevelDerivations: List[Derivation] = {
    val topLevelDerivationsWithMeta = jarLoaderService.jars.flatMap(
      jarEntry => { jarEntry._2.derivations.filterNot( d => elementBerekeningClass.isAssignableFrom( d.getClass ))}
    ).toList

    Logger.info(s"Detected the following top-level derivations: [${topLevelDerivationsWithMeta.map( _.getClass.getName ).mkString(", ")}]")

    topLevelDerivationsWithMeta.flatMap( _.berekeningen )
  }

}
