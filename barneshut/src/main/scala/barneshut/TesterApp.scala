package barneshut

import scala.collection.Seq

object TesterApp {
  def main(args: Array[String]) = {
    val b1 = new Body(1f, 18f, 26f, 0f, 0f)
    val b2 = new Body(2f, 142f, 312f, 0f, 0f)
    val b3 = new Body(3f, 31f, 234f, 0f, 0f)
    val b4 = new Body(4f, 42f, 32f, 0f, 0f)
    val b5 = new Body(4f, 56f, 9f, 0f, 0f)

    val l1 = Empty(0.5f,0.5f,1.0f)
    val l2 = Empty(1.5f,0.5f,1.0f)
    val l3 = Empty(0.5f,1.5f,1.0f)
    val l4 = Empty(1.5f,1.5f,1.0f)
    val f = Fork(l1, l2, l3, l4)
    println(f)
    val f1 = f.insert(b1)
    println(f1)
    val f2 = f1.insert(b2)
    println(f2)

    /*val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 101
    boundaries.maxY = 101
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    sm += b1
    sm += b2
    sm += b3
    sm += b4
    sm += b5
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    println(res)*/

  }
}
