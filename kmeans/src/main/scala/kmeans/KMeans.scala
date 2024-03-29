package kmeans

import scala.annotation.tailrec
import scala.collection.{Map, Seq, mutable}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.{ParMap, ParSeq}
import scala.util.Random
import org.scalameter._

class KMeans extends KMeansInterface {

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to(mutable.ArrayBuffer)
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to(mutable.ArrayBuffer)
  }

  def findClosest(p: Point, means: IterableOnce[Point]): Point = {
    val it = means.iterator
    assert(it.nonEmpty)
    var closest = it.next()
    var minDistance = p.squareDistance(closest)
    while (it.hasNext) {
      val point = it.next()
      val distance = p.squareDistance(point)
      if (distance < minDistance) {
        minDistance = distance
        closest = point
      }
    }
    closest
  }

  def classify(points: Seq[Point], means: Seq[Point]): Map[Point, Seq[Point]] = {
    if(points.isEmpty){
      means.map(point => (point, Seq())).toMap
    } else {
      points.groupBy(point => findClosest(point, means))
    }
  }

  def classify(points: ParSeq[Point], means: ParSeq[Point]): ParMap[Point, ParSeq[Point]] = {
    if(points.isEmpty){
      means.map(point => (point, ParSeq())).toMap
    } else {
      points.groupBy(point => findClosest(point, means))
    }
  }

  def findAverage(oldMean: Point, points: Seq[Point]): Point = if (points.isEmpty) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  def findAverage(oldMean: Point, points: ParSeq[Point]): Point = if (points.isEmpty) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.seq.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  def update(classified: Map[Point, Seq[Point]], oldMeans: Seq[Point]): Seq[Point] = {
    oldMeans.map(point => findAverage(point, classified(point)))
  }

  def update(classified: ParMap[Point, ParSeq[Point]], oldMeans: ParSeq[Point]): ParSeq[Point] = {
    oldMeans.map(point => findAverage(point, classified(point)))
  }

  def converged(eta: Double, oldMeans: Seq[Point], newMeans: Seq[Point]): Boolean = {
    oldMeans.zip(newMeans).forall(tpl => tpl._1.squareDistance(tpl._2) <= eta)
  }

  def converged(eta: Double, oldMeans: ParSeq[Point], newMeans: ParSeq[Point]): Boolean = {
    oldMeans.zip(newMeans).forall(tpl => tpl._1.squareDistance(tpl._2) <= eta)
  }

  /**
    * 1. Pick k points called means. This is called initialization.
    * 2. Associate each input point with the mean that is closest to it. We obtain k clusters of points,
    *     and we refer to this process as classifying the points.
    * 3. Update each mean to have the average value of the corresponding cluster.
    * 4. If the k means have significantly changed, go back to step 2. If they did not,
    *     we say that the algorithm converged.
    * 5. The k means represent different clusters -- every point is in the cluster corresponding to the closest mean.
    */
  //def classify(points: Seq[Point], means: Seq[Point]): Map[Point, Seq[Point]]
  //def update(classified: Map[Point, Seq[Point]], oldMeans: Seq[Point]): Seq[Point]
  //def converged(eta: Double, oldMeans: Seq[Point], newMeans: Seq[Point]): Boolean
  @tailrec
  final def kMeans(points: Seq[Point], means: Seq[Point], eta: Double): Seq[Point] = {
    val classified = classify(points, means)
    val updated = update(classified, means)
    if (!converged(eta, means, updated)) kMeans(points, updated, eta) else updated // your implementation need to be tail recursive
  }

  @tailrec
  final def kMeans(points: ParSeq[Point], means: ParSeq[Point], eta: Double): ParSeq[Point] = {
    val classified = classify(points, means)
    val updated = update(classified, means)
    if (!converged(eta, means, updated)) kMeans(points, updated, eta) else updated // your implementation need to be tail recursive
  }
}

/** Describes one point in three-dimensional space.
 *
 *  Note: deliberately uses reference equality.
 */
class Point(val x: Double, val y: Double, val z: Double) {
  private def square(v: Double): Double = v * v
  def squareDistance(that: Point): Double = {
    square(that.x - x)  + square(that.y - y) + square(that.z - z)
  }
  private def round(v: Double): Double = (v * 100).toInt / 100.0
  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"
}


object KMeansRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val kMeans = new KMeans()

    val numPoints = 500000
    val eta = 0.01
    val k = 32
    val points = kMeans.generatePoints(k, numPoints)
    val means = kMeans.initializeMeans(k, points)

    val seqtime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }

    val parPoints = points.par
    val parMeans = means.par

    val partime = standardConfig measure {
      kMeans.kMeans(parPoints, parMeans, eta)
    }

    // Additional `println` to avoid bad interaction with JLine output
    println()
    println()
    println()
    println()
    println(s"sequential time: $seqtime")
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
    println()
    println()
    println()
  }

  // Workaround Dotty's handling of the existential type KeyValue
  implicit def keyValueCoerce[T](kv: (Key[T], T)): KeyValue = {
    kv.asInstanceOf[KeyValue]
  }
}
