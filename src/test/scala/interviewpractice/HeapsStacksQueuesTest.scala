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


  test("decodeString 1") {
    decodeString("2[a]") shouldEqual "aa"
  }

  test("decodeString 2") {
    decodeString("b2[a]") shouldEqual "baa"
  }

  test("decodeString 3") {
    decodeString("2[a]3[b]4[c]5[d]") shouldEqual "aabbbccccddddd"
  }
  test("decodeString 4") {
    decodeString("x2[a]3[b]4[c]5[d]") shouldEqual "xaabbbccccddddd"
  }

  test("decodeString 5") {
    decodeString("x2[a]3[b]4[c]5[d]z") shouldEqual "xaabbbccccdddddz"
  }

  test("decodeString 6") {
    decodeString("2[abc]3[cd]ef") shouldEqual "abcabccdcdcdef"
  }

  test("decodeString 7") {
    decodeString("sd2[f2[e]g]i") shouldEqual "sdfeegfeegi"
  }

}
