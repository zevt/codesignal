package companies

import org.scalatest.funsuite.AnyFunSuite
import companies.Wizeline._
import org.scalatest.matchers.should.Matchers


class WizelineTest extends AnyFunSuite with Matchers {
//class WizelineTest extends FunSuite {

  test("test 1") {
    val a = Array("/project1/subproject1/method1",
      "/project2/subproject1/method1",
      "/project1/subproject1/method1",
      "/project1/subproject2/method1",
      "/project1/subproject1/method2",
      "/project1/subproject2/method1",
      "/project2/subproject1/method1",
      "/project1/subproject2/method1")

    countAPI(a) shouldEqual Array(
      "--project1 (6)",
      "----subproject1 (3)",
      "------method1 (2)",
      "------method2 (1)",
      "----subproject2 (3)",
      "------method1 (3)",
      "--project2 (2)",
      "----subproject1 (2)",
      "------method1 (2)")
  }

  test("test 2") {
    val a = Array("/main/subproject3/method2",
      "/main/subproject3/method2",
      "/project5/subproject2/method5",
      "/project4/subproject1/method2",
      "/project3/subproject3/method1",
      "/project3/subproject1/method10",
      "/project4/subproject1/method2",
      "/project3/subproject2/method3",
      "/project2/subproject7/method3",
      "/main/subproject6/method5")

    countAPI(a) shouldEqual Array("--main (3)",
      "----subproject3 (2)",
      "------method2 (2)",
      "----subproject6 (1)",
      "------method5 (1)",
      "--project5 (1)",
      "----subproject2 (1)",
      "------method5 (1)",
      "--project4 (2)",
      "----subproject1 (2)",
      "------method2 (2)",
      "--project3 (3)",
      "----subproject3 (1)",
      "------method1 (1)",
      "----subproject1 (1)",
      "------method10 (1)",
      "----subproject2 (1)",
      "------method3 (1)",
      "--project2 (1)",
      "----subproject7 (1)",
      "------method3 (1)")
  }


}
