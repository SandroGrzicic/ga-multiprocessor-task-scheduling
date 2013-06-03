package hr.fer.sandrogrzicic.konpro.aktivnost

import collection.immutable.IndexedSeq
import com.typesafe.config.ConfigFactory
import net.sandrogrzicic.ga.Chromosome

/**
 * A Task chromosome. Immutable.
 */
case class TaskChromosome (
    chromosome: IndexedSeq[IndexedSeq[Task]],
    processorDelayTime: Int
) extends Chromosome[TaskChromosome] {

  if (!checkOrder) {
    throw new IllegalArgumentException("Invalid task order!")
  }
  if (!checkUniqueness) {
    throw new IllegalArgumentException("Multiple tasks with identical IDs found!")
  }

  /** Maximum height of all tasks in this TaskChromosome. */
  lazy val maxHeight = chromosome.foldLeft(0) { case (max, processor) => math.max(max, processor.last.height) }

  /**
   * Attempts a crossover of this and the given chromosome and returns the resulting children.
   *
   * @param other the other parent chromosome.
   * @return a pair of children chromosomes.
   */
  override def crossover(other: TaskChromosome) = {
    val randomHeight = rng.nextInt(maxHeight)

    val processorTasksZipped = chromosome.zip(other.chromosome)

    val child1tasks = processorTasksZipped.map { case (p1, p2) =>
      p1.takeWhile(_.height < randomHeight) ++ p2.dropWhile(_.height < randomHeight)
    }
    val child2tasks = processorTasksZipped.map { case (p1, p2) =>
      p2.takeWhile(_.height < randomHeight) ++ p1.dropWhile(_.height < randomHeight)
    }

    (copy(chromosome = child1tasks), copy(chromosome = child2tasks))
  }

  /**
   * Returns a mutated version of this chromosome.
   * If only a small percentage of all tasks (or none) have identical heights,
   * the mutation can fail and the unchanged chromosome will be returned.
   */
  override def mutate: TaskChromosome = {
    val processorIndex = rng.nextInt(chromosome.length)
    val processor = chromosome(processorIndex)
    val taskIndex = rng.nextInt(processor.length - 1)
    val taskIndexNext = taskIndex + 1

    for (_ <- 0 until processor.length) {
      if (processor(taskIndex).height == processor(taskIndexNext).height) {
        return copy(chromosome = chromosome.updated(processorIndex,
              processor.updated(taskIndex, processor(taskIndexNext)).updated(taskIndexNext, processor(taskIndex))
          ))
      }
    }
    this
  }

  /** Checks if each task appears only once. */
  lazy val checkUniqueness = {
    val merged = chromosome.reduce { _ ++ _ }
    merged.distinct.size == merged.size
  }

  /** Checks whether tasks are in order (using task height). */
  lazy val checkOrder: Boolean = {
    (for (tasks <- chromosome) yield {
      tasks.foldLeft(tasks(0).height) {
        case (height, task) => if (height > task.height) -1 else task.height
      }
    }) != -1
  }

  lazy val sumOfTasks = chromosome.foldLeft(0) { case (sum, tasks) => sum + tasks.length }

  /** Sum of all task durations and communication delays. */
  lazy val totalDurationsDelays = chromosome.foldLeft(0) {
    case (totalSum, tasks) => totalSum + tasks.foldLeft(0) { case (sum, task) => sum + task.time + task.delay + processorDelayTime }
  }

  /** Returns the fitness of this chromosome. */
  lazy val fitness = {
    /** Per-processor maximum times */
    val maximums = new Array[Int](chromosome.length)
    val taskTimes = new collection.mutable.HashMap[Task, Int]

    // for each processor...
    for (p <- 0 until chromosome.length) {
      // for each task of processor p...
      for (task <- chromosome(p)) {
        // update current task time
        taskTimes += task -> (task.previous match {
          case None           => maximums(p) + task.time
          case Some(prevTask) =>
            if (chromosome(p).contains(prevTask))
              maximums(p) + task.time + math.max(maximums(p), taskTimes.getOrElse(prevTask, 0))
            else
              maximums(p) + task.time + task.delay + processorDelayTime + math.max(maximums(p), taskTimes.getOrElse(prevTask, 0))
        })
        // update processor total task time
        maximums(p) += taskTimes(task)
      }
    }

    // fitness = 1.0 / (longest time between all processors)
//    1.0 / maximums.max
    totalDurationsDelays.toDouble / maximums.max
  }

  override def toString = {
    val sb = StringBuilder.newBuilder
    for ((processorTasks, processorIndex) <- chromosome.zip(1 to chromosome.length)) {
      sb.append(processorIndex).append(") ")
      for (task <- processorTasks) {
        sb.append(task).append(" -> ")
      }
      if (sb.length > 4) sb.setLength(sb.length - 4)
      sb.append("\n")
    }
    sb.toString()
  }

}

object TaskChromosome {
  val conf = ConfigFactory.load("geneticalgorithms")
  val NUMBER_OF_PROCESSORS = conf.getInt("ga.tasks.number-of-processors")
  val PROCESSOR_DELAY_TIME = conf.getInt("ga.tasks.processor-delay-time")
  val rng = new scala.util.Random()

  def max(tasks: IndexedSeq[Task]) = tasks.maxBy(_.height)
  def maxHeight(tasks: IndexedSeq[Task]) = tasks.foldLeft(0) { case (max, task) => math.max(max, task.height) }

  def apply(processorDelayTime: Int, tasks: IndexedSeq[Task]*) = {
    new TaskChromosome(tasks.toIndexedSeq, processorDelayTime)
  }
  /**
   * Fills the given number of processors with the given tasks grouped by height
   * and returns the constructed chromosome.
   */
  def apply(tasks: IndexedSeq[Task], numberOfProcessors: Int = NUMBER_OF_PROCESSORS, processorDelayTime: Int = PROCESSOR_DELAY_TIME) = {
    val tasksMaxHeight = tasks.maxBy(_.height).height + 1

    // group the given tasks into "buckets" sorted by height
    val groupedByHeight = tasks.foldLeft(IndexedSeq.fill(tasksMaxHeight)(IndexedSeq.empty[Task])) {
      case (seq, t) => seq.updated(t.height, seq(t.height) :+ t)
    }

    // group the tasks sorted by height into processor "buckets"
    val groupedByProcessor = groupedByHeight.flatten.zipWithIndex.foldLeft(IndexedSeq.fill(numberOfProcessors)(IndexedSeq.empty[Task])) {
      case (seq, (t, index)) => seq.updated(index % numberOfProcessors, seq(index % numberOfProcessors) :+ t)
    }

    new TaskChromosome(groupedByProcessor, processorDelayTime)
  }

  /**
   * Generates and returns a new population of TaskChromosomes.
   */
  def generatePopulation(
    populationSize: Int, tasks: IndexedSeq[Task],
    numberOfProcessors: Int = NUMBER_OF_PROCESSORS, processorDelayTime: Int = PROCESSOR_DELAY_TIME
  ) = {
    Vector.fill(populationSize)(TaskChromosome(rng.shuffle(tasks), numberOfProcessors, processorDelayTime))
  }
}
