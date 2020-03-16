package interviewpractice

import interviewpractice.HeapsStacksQueues._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
/**
 * @author Viet Quoc Tran
 *         on 3/15/20.
 * www.zeroexception.com
 */
class HeapsStacksQueuesTest extends AnyFunSuite with Matchers {

  test("kthLargestElement") {
    kthLargestElement(Array(1,2,3,4), 2) shouldEqual 3
  }

  test("kthLargestElement 1") {
    kthLargestElement(Array(1,4,3,2), 2) shouldEqual 3
  }

  test("kthLargestElement 2") {
    kthLargestElement(Array(1,4,3,2), 1) shouldEqual 4
  }
}
