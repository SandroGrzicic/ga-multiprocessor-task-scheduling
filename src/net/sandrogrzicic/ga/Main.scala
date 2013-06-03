package net.sandrogrzicic.ga

import collection.immutable.IndexedSeq

/**
 * Implementation of a genetic algorithm.
 */
object Main extends App {
  val rng = new scala.util.Random()

  def timed(computation: => Unit) {
    val start = System.currentTimeMillis()
    computation
    println("\n" + (System.currentTimeMillis() - start) + " ms")
  }

  timed {
    primitiveSeqTest()
  }

  def primitiveSeqTest() {
    val fitnessFunction = { seq: IndexedSeq[Int] =>
      var acc = 0
      var prev = seq(0)
      // sort min to max
      for (cur <- seq.tail) { if (prev <= cur) acc += 1; prev = cur }
      acc.toDouble / (seq.size-1)
    }
//    val numbers = (0 until 20).map(_ => rng.nextDouble()).toVector
    val numbers = (0 until 15).toVector

    val popSize = 20
    val pop = Population(
      Vector.fill(popSize)(PrimitiveSeqChromosome(rng.shuffle(numbers), fitnessFunction)),
      SelectionFunctions.tournamentSelection()
    )

    reproduce(pop, 10, 90, 9900, 10000, 30000, 50000)
//    reproduceUntilFitness(pop, 0.95)

  }

  /**
   * Execute the GA reproduction until desired fitness is reached.
   */
  def reproduceUntilFitness[C <: Chromosome[C]](initialPopulation: Population[C], desiredFitness: Double) {
    if (desiredFitness < 0.0 || desiredFitness > 1.0) throw new IllegalArgumentException("Invalid fitness!")

    var pop = initialPopulation
    printPopulation(pop)
    var generation = 0
    while (pop.mostFit.fitness < desiredFitness) {
      pop = pop.reproduce()
      generation += 1
    }
    printPopulation(pop)
    println("Total generations: " + generation)
  }

  /**
   * Execute the GA reproduction a set number of times.
   */
  def reproduce[C <: Chromosome[C]](initialPopulation: Population[C], repeatCounts: Int*) = {
    var pop = initialPopulation
    var generation = 0
    for (count <- repeatCounts) {
      println("----------------------- Generation: " + generation + "\t")
      printPopulation(pop)
      for (_ <- 0 to count) { pop = pop.reproduce() }
      generation += count
    }
    println("----------------------- Generation: " + generation + "\t")
    printPopulation(pop)
    pop
  }

  /** Print the current population and best fitness. */
  def printPopulation[C <: Chromosome[C]](p: Population[C]) {
    print("" + p.leastFit + p.mostFit)
    printPopulationFitness(p)
  }
  def printPopulationFitness[C <: Chromosome[C]](p: Population[C]) {
    println("\tfitness: =(" + p.leastFit.fitness.toFloat + " ~ " + p.mostFit.fitness.toFloat + ")=")
  }

}
