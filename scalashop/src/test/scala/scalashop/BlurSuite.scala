package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  @Test def `testSimpleBoxBlure`: Unit = {
    val i: Img = new Img(3, 3, List(2,2,2,2,1,2,2,2,2).toArray)
    val r = boxBlurKernel(i,0,0,1)
    val e: RGBA = 1

    assert(e == r, "expected 1 but was " + r)
  }

  @Test def `testSimpleBoxBlure2`: Unit = {
    val i2: Img = new Img(3, 3, List(2,2,2,2,1,4,4,4,4).toArray)
    val r2 = boxBlurKernel(i2,1,1,1)
    assert(2 == r2, "expected 2 but was " + r2)
  }
}
