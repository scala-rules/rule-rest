package services

import javax.inject.{Inject, Singleton}

import org.scalarules.derivations.Derivation
import org.scalarules.dsl.nl.grammar.{Berekening, ElementBerekening}
import play.api.{Configuration, Logger}

@Singleton
class DerivationsService @Inject() (configuration: Configuration, jarLoaderService: JarLoaderService) {

  val derivationsWithMeta: List[DerivationHolder[_ <: Berekening]] = jarLoaderService.jars.flatMap(jarEntry => {
    jarEntry._2.derivations.map( d => new DerivationHolder(d.berekeningen, d.getClass) )
  }).toList

  val derivations: List[Derivation] = derivationsWithMeta.flatMap( _.derivations )

  lazy val elementBerekeningClass = classOf[ElementBerekening[Any, Any]]
  lazy val topLevelDerivations: List[Derivation] = {
    val topLevelDerivationsWithMeta = derivationsWithMeta.filter( d => !elementBerekeningClass.isAssignableFrom( d.originalClass ) )

    Logger.info(s"Detected the following top-level derivations: [${topLevelDerivationsWithMeta.map( _.originalClass.getName ).mkString(", ")}]")

    topLevelDerivationsWithMeta.flatMap( _.derivations )
  }

}

class DerivationHolder[T <: Berekening](val derivations: List[Derivation], val originalClass: Class[T])

