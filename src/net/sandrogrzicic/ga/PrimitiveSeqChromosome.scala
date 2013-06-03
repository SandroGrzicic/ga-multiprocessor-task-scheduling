package net.sandrogrzicic.ga

import collection.immutable.IndexedSeq

/**
 * A Chromosome of a Seq of primitives.
 */
case class PrimitiveSeqChromosome[T](
    seq: IndexedSeq[T],
    fitnessFunction: IndexedSeq[T] => Double
) extends Chromosome[PrimitiveSeqChromosome[T]] {

  override def crossover(other: PrimitiveSeqChromosome[T]) = {
//    val other = otherAbstract.asInstanceOf[this.type]
//
//    val cutoff = rng.nextInt(seq.length)
//
//    val child1 = seq.drop(cutoff) ++ seq.take(cutoff)
//    val child2 = other.seq.drop(cutoff) ++ other.seq.take(cutoff)
//
//    (copy(seq = child1), copy(seq = child2))
    (this, other)
  }

  override def mutate = {
    val (el1, el2) = (IndexedElement(rng.nextInt(0, seq.length)), IndexedElement(rng.nextInt(0, seq.length)))
    if (el1 != el2)
      copy(seq = seq.updated(el1.index, el2.element).updated(el2.index, el1.element))
    else
      this
  }

  case class IndexedElement(index: Int) { def element = seq(index) }

  def fitness = fitnessFunction(seq)

  override lazy val toString = {
    val sb = StringBuilder.newBuilder
    sb.append("PrimitiveSeqChromosome[").append(seq.mkString(", ")).append("]\n")
    sb.toString()
  }
}
