package interviewpractice

/**
 * @author Viet Quoc Tran
 *         on 3/15/20.
 * www.zeroexception.com
 */
object HeapsStacksQueues {

//  https://app.codesignal.com/interview-practice/task/BG94ZFECSNo6Kv7XW
  def kthLargestElement(nums: Array[Int], k: Int): Int = {
    val q = Array.fill(k)(Int.MinValue)

    for (e <- nums) {
      if (q.head < e) {
        var i = 0
        while (i < k && e > q(i)) {
          i += 1
        }
        for (j <- 0 until i - 1) {
          q(j) = q(j + 1)
        }
        q(i - 1) = e
      }
    }
    q.head
  }

//  https://app.codesignal.com/interview-practice/task/aRwxhGcmvhf6vKPCp
  def simplifyPath(path: String): String = {
    ""
  }


}
