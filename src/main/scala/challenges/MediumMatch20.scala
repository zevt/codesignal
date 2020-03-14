package challenges

/**
 * @author Viet Quoc Tran
 *         on 3/14/20.
 * www.zeroexception.com
 */
object MediumMatch20 {

//  https://app.codesignal.com/challenge/KHYiHC5rvsP2f3poh
def chessQueen(q: String): Array[String] = {
  val n = Array.range(1,9)
  var a = Array[String]()
  Array.range(0, 8)
    .map(_ + 'a').map(_.asInstanceOf[Char])
    .flatMap(e => n.map(e.toString + _))
    .foreach(p => {
      if (!(p(0) == q(0) || p(1) == q(1) || Math.abs(p(0) - q(0)) == Math.abs(p(1)-q(1))))
        a = a :+ p
    })
  a
}


}
