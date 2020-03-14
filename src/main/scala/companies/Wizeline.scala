package companies


object Wizeline {

//  https://app.codesignal.com/company-challenges/wizeline/toyP6DzaKAPq2Yhma
  def countAPI(calls: Array[String]): Array[String] = {

    val a = calls.map(_.split("/").tail)
    Forest(a).get
  }

}

case class Forest(a: Array[Array[String]], level:Int = 1) {
  val map = scala.collection.mutable.Map[String,(Forest, Int)]().withDefaultValue((null, 0))
  var order = Array[String]()

  parse

  def parse: Forest = {
    val tmap = scala.collection.mutable.Map[String, (Array[Array[String]], Int)]().withDefaultValue(Array(), 0)
    if (a.length > 0 && a.head.length > 1) {
      for (parts <- a) {
        order = order :+ parts.head
        tmap.update(parts.head, (tmap(parts.head)._1 :+ parts.tail, tmap(parts.head)._2 + 1))
      }
      for ((k, v) <- tmap) {
        map.update(k, (Forest(v._1, level + 1), v._2))
      }
    } else {
      for (part <- a) {
        map.update(part.head, (null, map(part.head)._2 + 1))
        order = order :+ part.head
      }
    }
    this
  }

  def get: Array[String] = {
    var result = Array[String]()
    var i = 0;
    while (map.nonEmpty) {
      val key = order(i)
      if (map.contains(key)) {
        result = result :+ (List.fill(level)("--").mkString + key + " (" + map(key)._2 + ")")
        if (map(key)._1 != null) {
          result = result ++ map(key)._1.get
        }
        map.remove(key)
      }
      i += 1
    }
    result
  }
}

