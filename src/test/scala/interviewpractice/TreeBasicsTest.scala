package interviewpractice

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import interviewpractice.TreeBasics._

/**
 * @author Viet Quoc Tran
 *         on 3/15/20.
 * www.zeroexception.com
 */
class TreeBasicsTest extends AnyFunSuite with Matchers {

  test("findSubStrings") {
    val words = Array("neuroses",
      "myopic",
      "sufficient",
      "televise",
      "coccidiosis",
      "gules",
      "during",
      "construe",
      "establish",
      "ethyl")
    val parts = Array("aaaaa",
    "Aaaa",
    "E",
    "z",
    "Zzzzz",
    "a",
    "mel",
    "lon",
    "el",
    "An",
    "ise",
    "d",
    "g",
    "wnoVV",
    "i",
    "IUMc",
    "P",
    "KQ",
    "QfRz",
    "Xyj",
    "yiHS")

    val expected = Array("neuroses",
      "myop[i]c",
      "suff[i]cient",
      "telev[ise]",
      "cocc[i]diosis",
      "[g]ules",
      "[d]uring",
      "construe",
      "est[a]blish",
      "ethyl")
    findSubstrings(words, parts) shouldEqual expected
  }

  test("deleteFromBST 1") {
    val left = Tree(2)
    left.right = Some(Tree(3))
    val right = Tree(5)
    val tree = Tree(4)
    tree.left = Some(left)
    tree.right = Some(right)

    val expectedTree = Tree(4)
    expectedTree.left = Some(Tree(2))
    expectedTree.right = Some(Tree(5))

    val actual = deleteFromBST(Some(tree), Array(3,2,4,5))

     actual shouldEqual None
  }

  test("deleteFromBST 2") {
    val left = Tree(2)
    left.right = Some(Tree(3))
    val right = Tree(5)
    val tree = Tree(4)
    tree.left = Some(left)
    tree.right = Some(right)

    val expectedTree = Tree(4)
    expectedTree.left = Some(Tree(3))
    expectedTree.right = Some(Tree(5))

    val actual = deleteFromBST(Some(tree), Array(2))

    actual shouldEqual Some(expectedTree)
  }

  test("deleteFromBST 3") {
    val left = Tree(2)
    left.right = Some(Tree(3))
    val right = Tree(5)
    val tree = Tree(4)
    tree.left = Some(left)
    tree.right = Some(right)

    val expectedTree = Tree(4)
    expectedTree.left = Some(Tree(2))
    expectedTree.left.get.right = Some(Tree(3))
    expectedTree.right = Some(Tree(5))

    val actual = deleteFromBST(Some(tree), Array(7))

    actual shouldEqual Some(expectedTree)
  }

  test("deleteFromBST 4") {

    val tree = Tree(5)
    val left = Tree(2)
    left.left = Some(Tree(1))
    left.right = Some(Tree(3))
    tree.left = Some(left)


    val right = Tree(6)
    right.right = Some(Tree(8))
    right.right.get.left = Some(Tree(7))
    tree.right = Some(right)

    val expectedTree = Tree(3)
    expectedTree.left = Some(Tree(2))
    expectedTree.left.get.left = Some(Tree(1))

    expectedTree.right = Some(Tree(8))
    expectedTree.right.get.left = Some(Tree(7))

    val actual = deleteFromBST(Some(tree), Array(4,5,6))

    actual shouldEqual Some(expectedTree)
  }


  test("deleteFromBST 5") {

    val tree = Tree(3)
    val left = Tree(2)
    left.left = Some(Tree(1))
    tree.left = Some(left)


    val right = Tree(5)
    tree.right = Some(right)

    val expectedTree = Tree(5)

    val actual = deleteFromBST(Some(tree), Array(3,2,1))

    actual shouldEqual Some(expectedTree)
  }

}
