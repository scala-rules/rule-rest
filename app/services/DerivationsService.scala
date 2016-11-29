package services

import javax.inject.{Inject, Singleton}

import org.scalarules.dsl.nl.grammar.Berekening
import org.scalarules.engine.Derivation
import play.api.Configuration

@Singleton
class DerivationsService @Inject() (configuration: Configuration, jarLoaderService: JarLoaderService) {

  val derivations: List[Derivation] = jarLoaderService.jars.flatMap(jarEntry => {
    jarEntry._2.derivations.map( d => d.berekeningen)
  }).toList.flatten

}
