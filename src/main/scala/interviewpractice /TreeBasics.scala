package interviewpractice

import java.util.Objects

/**
 * @author Viet Quoc Tran vt 
 *         on 3/15/20.
 * www.zeroexception.com
 */
object TreeBasics {


//  https://app.codesignal.com/interview-practice/task/jAKLMWLu8ynBhYsv6

//   Binary trees are already defined with this interface:
   case class Tree[T](x : T) {
     var value: T = x
     var left: Option[Tree[T]] = None
     var right: Option[Tree[T]] = None

    override def equals(obj: Any): Boolean = {
      if (!obj.isInstanceOf[Tree[T]]) {
        false
      }  else {
        val other = obj.asInstanceOf[Tree[T]]
        Objects.equals(value,other.value) && left.equals(other.left) && right.equals(other.right)
      }
    }
}

  def kthSmallestInBST(t: Option[Tree[Int]], k: Int): Int = {
    val leftSize = size(t.get.left)
    if ( leftSize >= k) {
      kthSmallestInBST(t.get.left, k)
    } else if (leftSize + 1 == k) {
      t.get.value
    } else {
      kthSmallestInBST(t.get.right, k - leftSize - 1)
    }
  }

  def size(t: Option[Tree[Int]]): Int = {
   if (t.isEmpty) {
     0
   } else {
     1 + size(t.get.left) + size(t.get.right)
   }
  }


//  https://app.codesignal.com/interview-practice/task/mDpAJnDQkJqaYYsCg
//
// Binary trees are already defined with this interface:
//   class Tree[T](x : T) {
//     var value: T = x
//     var left: Option[Tree[T]] = None
//     var right: Option[Tree[T]] = None
//   }
//
  def isSubtree(t1: Option[Tree[Int]], t2: Option[Tree[Int]]): Boolean = {
    if (t1.nonEmpty) {
      if (equalTree(t1, t2) || isSubtree(t1.get.left, t2) || isSubtree(t1.get.right, t2)) {
        true
      }
      else {
        false
      }
    } else if (t2.isEmpty) {
        true
    }else {
      false
    }
  }

  def equalTree(t1: Option[Tree[Int]], t2: Option[Tree[Int]]): Boolean = {
    if (t1.isEmpty) {
      t2.isEmpty
    } else if (t2.nonEmpty){
      t1.get.value == t2.get.value && equalTree(t1.get.left, t2.get.left) && equalTree(t1.get.right, t2.get.right)
    }else {
      false
    }
  }


//  https://app.codesignal.com/interview-practice/task/AaWaYxi8gjtbqgp2M/description
  def restoreBinaryTree(inorder: Array[Int], preorder: Array[Int]): Option[Tree[Int]] = {
    if (preorder.isEmpty) {
      None
    } else if (preorder.length == 1) {
      val root = new Tree[Int](preorder.head)
      root.left = None
      root.right = None
      Some(root)
    } else {
      val root = new Tree[Int](preorder(0))
      val ind = inorder.indexOf(root.value)
      val inorderLeft = inorder.slice(0, ind)
      val inorderRight = inorder.slice(ind + 1, inorder.length)
      val preLeft = preorder.slice(1, 1 + inorderLeft.length)
      val preRight = preorder.slice(ind + 1, preorder.length)
      root.left = restoreBinaryTree(inorderLeft, preLeft)
      root.right = restoreBinaryTree(inorderRight, preRight)
      Some(root)
    }
  }

//  https://app.codesignal.com/interview-practice/task/Ki9zRh5bfRhH6oBau
  def findSubstrings(words: Array[String], parts: Array[String]): Array[String] = {
    words.map(w => {
      var sub = ""
      var i = w.length
      for (s <- parts) {
        val ind = w.indexOf(s)
        if (ind > -1 && (s.length > sub.length || (s.length == sub.length && ind < i))) {
          sub = s
          i = ind
        }
      }
      if (sub.length > 0) {
        val ind = w.indexOf(sub)
        w.substring(0, ind) + "[" + sub + "]" + w.substring(ind + sub.length)
      } else {
        w
      }
    })
  }

//  https://app.codesignal.com/interview-practice/task/oZXs4td52fsdWC9kR
  def deleteFromBST(t: Option[Tree[Int]], queries: Array[Int]): Option[Tree[Int]] = {

    def removeKey(t: Option[Tree[Int]], key: Int): Option[Tree[Int]] = {
      var tt = t
      if (tt.isEmpty) None
      else {
        if (tt.get.value == key) {
          if (tt.get.left.isEmpty) {
            tt = t.get.right
          } else {
            var nRoot = tt
            var n = t.get.left
            var movable = true
            while (n.nonEmpty && movable) {
              if (n.get.right.nonEmpty) {
                nRoot = n
                n = n.get.right
              } else {
                movable = false
              }
            }
            if (n == nRoot.get.right) {
              nRoot.get.right = n.get.left
              tt.get.value = n.get.value
            } else {
              nRoot = tt.get.left
              nRoot.get.right = tt.get.right
              tt = nRoot
            }
          }
        } else if (tt.get.value < key) {
          tt.get.right = removeKey(tt.get.right, key)
        }else {
          tt.get.left = removeKey(tt.get.left, key)
        }
        tt
      }
    }

    var ind = 0
    var tt = t
    while (tt.nonEmpty && ind < queries.length) {
      tt = removeKey(tt, queries(ind))
      ind += 1
    }
    tt
  }




}
