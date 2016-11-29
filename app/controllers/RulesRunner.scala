package controllers

import org.scalarules.engine._

object RulesRunner {
  def run(context: Context, derivations: List[Derivation]): Context = FactEngine.runNormalDerivations(context, derivations)
  def runDebug(context: Context, derivations: List[Derivation]): (Context, List[Step]) = FactEngine.runDebugDerivations(context, derivations)
}
