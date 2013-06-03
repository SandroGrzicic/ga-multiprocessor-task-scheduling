package net.sandrogrzicic.ga

import com.typesafe.config.ConfigFactory

/**
 * A chromosome.
 */
trait Chromosome[ChromosomeType <: Chromosome[ChromosomeType]] {
  import Chromosome._
  val rng = java.util.concurrent.ThreadLocalRandom.current()

  /** Implemented by the concrete Chromosome. */
  def crossover(other: ChromosomeType): (ChromosomeType, ChromosomeType)
  /** Implemented by the concrete Chromosome. */
  def mutate: ChromosomeType

  /**
   * Returns a pair of chromosomes which have been crossed over using this and the given chromosome,
   * or the original pair, depending on the crossover chance.
   */
  def crossoverMaybe(other: ChromosomeType) = {
    if (rng.nextFloat() < CROSSOVER_CHANCE) {
      crossover(other)
    } else {
      (this.asInstanceOf[ChromosomeType], other)
    }
  }

  /**
   * Returns a possibly mutated version of this chromosome, depending on the mutation chance.
   */
  def mutateMaybe = {
    if (rng.nextFloat() < MUTATION_CHANCE) {
      mutate
    } else {
      this.asInstanceOf[ChromosomeType]
    }
  }

  /** Returns the fitness of this chromosome. */
  def fitness: Double

}

object Chromosome {
  val conf = ConfigFactory.load("geneticalgorithms")

  val CROSSOVER_CHANCE = conf.getDouble("ga.crossover-chance").toFloat
  val MUTATION_CHANCE = conf.getDouble("ga.mutation-chance").toFloat

}
