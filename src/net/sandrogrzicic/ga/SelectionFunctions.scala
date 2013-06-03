package net.sandrogrzicic.ga

import collection.immutable.IndexedSeq
import java.util.concurrent.ThreadLocalRandom

/**
 * Available selection functions.
 */
object SelectionFunctions {
  /** The signature all selection functions need to satisfy. */
  type SelectionFunction[C <: Chromosome[C]] = IndexedSeq[C] => C

  /**
   * Tournament selection. http://en.wikipedia.org/wiki/Tournament_selection
   * @param k tournament size, number of randomly chosen individuals, 3 by default
   * @return the selected individual
   */
  def tournamentSelection[C <: Chromosome[C]](k: Int = 3): SelectionFunction[C] = {
    population: IndexedSeq[C] =>
      val rng = ThreadLocalRandom.current()
      val indexes = for (_ <- 0 until k) yield { rng.nextInt(population.size) }
      val selected = for (i <- indexes) yield { population(i) }
      val sorted = selected.sortWith(_.fitness < _.fitness)
      sorted.last
  }

}
