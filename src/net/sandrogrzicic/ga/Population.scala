package net.sandrogrzicic.ga

import collection.immutable.IndexedSeq
import SelectionFunctions.SelectionFunction
import scala.reflect.ClassTag

/**
 * A chromosome population.
 */
case class Population[C <: Chromosome[C] : ClassTag](
    population: IndexedSeq[C],
    selectionFunction: SelectionFunction[C] = SelectionFunctions.tournamentSelection(),
    useElitism: Boolean = true
) {
  val rng = java.util.concurrent.ThreadLocalRandom.current()

  if (population.size % 2 == 1)
    throw new IllegalArgumentException("Population size must be even!")

  /**
   * @return a population of children of the current generation.
   */
  def reproduce(): Population[C] = {

    val children = new Array[C](population.size)

    // runs in parallel
    for (i <- (0 until population.size by 2).par) {
      val parent1 = selectionFunction(population)
      val parent2 = selectionFunction(population.filter(_ != parent1))

      val (child1: C, child2: C) = parent1.crossover(parent2)

      children(i)   = child1.mutateMaybe
      children(i+1) = child2.mutateMaybe
    }

    if (useElitism) {
      val weakestChild = population.zipWithIndex.foldLeft(FitnessIndex(0, population(0).fitness)) {
        case (weakest, (c, index)) =>
          if (c.fitness < weakest.fitness) FitnessIndex(index, population(index).fitness) else weakest
      }
      // replace weakest child with best individual from parent generation
      children(weakestChild.index) = mostFit
    }

    Population(children.toVector, selectionFunction)
  }

  /** Data class; contains the index of an individual and its fitness. */
  case class FitnessIndex(index: Int, fitness: Double)

  /** The most fit individual in the population. */
//  lazy val mostFit = population.reduceLeft((c1, c2) => if (c1.fitness > c2.fitness) c1 else c2)
  lazy val mostFit = population.maxBy(_.fitness)

  /** The least fit individual in the population. */
  lazy val leastFit = population.minBy(_.fitness)

  override def toString = {
    val sb = StringBuilder.newBuilder
    sb.append("Population{\n")
    population.foreach { c => sb.append(c.toString) }
    sb.append("} fitness: (").append(leastFit.fitness).append(", ").append(mostFit.fitness).append(")\n")
    sb.toString()
  }

}

