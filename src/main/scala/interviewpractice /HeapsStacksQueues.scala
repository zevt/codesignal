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
    var p = path
    var clean = false
    do {
      val x = p.replaceAll("([/]{2})|(/\\./)", "/")
      clean = x.length == p.length
      p = x
    }
    while (!clean)
    val a = p.split("/")
    var r = Array[String]()
    for (s <- a) {
      if (s.equals("..")) {
        if (r.length > 0) {
          r = r.tail
        }
      } else {
        r = s +: r
      }
    }
    val result = r.reverse.mkString("/")
    if (!result.startsWith("/")) {
      "/" + result
    } else {
      result
    }

  }

//  https://app.codesignal.com/interview-practice/task/dYCH8sdnxGf5aGkez
  def decodeString(s: String): String = {

    var st = Array[Any]()
    var i = 0
    while (i < s.length) {
      var number = Array[Char]()
      while (i < s.length && s(i) >= '0' && s(i) <= '9') {
         number = s(i) +: number
        i += 1
      }
      if (number.nonEmpty) {
        st = number.reverse.mkString.toInt +: st
      }
      var word = Array[Char]()
      while (i < s.length && s(i) >= 'a' && s(i) <= 'z') {
        word = s(i) +: word
        i += 1
      }
      if (word.nonEmpty) {
        st = word.reverse.mkString +: st
      }
      if (i < s.length && s(i) == '[') {
        i += 1
      } else if (i < s.length && s(i) == ']') {
        var w = st.head.asInstanceOf[String]
        st = st.tail
        while (st.nonEmpty && st.head.isInstanceOf[String]) {
          w = st.head + w
          st = st.tail
        }
        val n = st.head.asInstanceOf[Int]
        st = st.tail
        var currentW = Array.fill(n)(w).mkString

        while (st.nonEmpty && !st.head.isInstanceOf[Int]) {
          currentW = st.head.asInstanceOf[String] + currentW
          st = st.tail
        }
        st = currentW +: st
        i += 1
      }
    }
    st.reverse.mkString
  }

//  TODO: Simplify this solution
//  def decodeString2(s: String): String = {
//    var number = Array[Char]()
//    var word = Array[Char]()
//    var st = Array[Any]()
//    for (c <- s) {
//      if (c >= 'a' && c <= 'z') {
//        if (number.nonEmpty) {
//          st = number.reverse.toString.toInt +: st
//          number = Array.empty
//        }
//        word = c +: word
//      } else if (c >= '0' && c <= '9') {
//        if (word.nonEmpty) {
//          st = word.reverse.toString +: st
//          word = Array.empty
//        }
//        number = c +: number
//      } else if (c == '[') {
//        if (number.nonEmpty) {
//          st = number.reverse.toString.toInt +: st
//          number = Array.empty
//        }
//      } else {
//
//      }
//
//    }
//  }

}
