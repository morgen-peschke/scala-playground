import Math._
import scala.language.postfixOps

sealed trait BinaryTree[A]

object BinaryTree {
  case object Empty extends BinaryTree[Nothing]

  case class Node[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A] {
    def isLeaf: Boolean =
      this match {
        case Node(_, Empty, Empty) => true
        case _                     => false
      }
  }

  object Node {
    def apply[A](value: A): Node[A] = new Node(value, Empty, Empty)

    def apply[A](value: A, left: A, right: A): Node[A] =
      new Node(
        value,
        Node(left),
        Node(right)
      )

    /*def leftOnly[A](value: A, left: A): Node[A] =
      new Node[A](value, Node(left), Empty)*/

    def rightOnly[A](value: A, right: A): Node[A] =
      new Node(value, Empty, Node(right))

    def rightOnly[A](value: A, right: Node[A]): Node[A] =
      new Node(value, Empty, right)
  }

  object IsLeaf {
    def unapply[A](t: BinaryTree[A]): Option[Node[A]] =
      t match {
        case n @ Node[A](_, _: Empty, _: Empty) => n.some
        case _                                  => None
      }
  }

  def traverseInOrder[A](root: BinaryTree[A], fn: Node[A] => Unit): Unit =
    root match {
      case Empty                              => ()
      case n @ Node[A](_, _: Empty, _: Empty) => fn(n)
      case n @ Node[A](_, left, right) =>
        traverseInOrder[A](left, fn)
        fn(n)
        traverseInOrder[A](right, fn)
    }

  def isBalanced[A](root: BinaryTree[A]): Boolean =
    height(root) != -2

  def height[A](root: BinaryTree[A]): Int =
    root match {
      case Empty => -1
      case n @ Node(_, left, right) =>
        val lh = height(left)
        if (lh == -2) -2
        else {
          val rh = height(right)
          if (rh == -2) -2
          else if (abs(lh - rh) > 1) -2
          else max(lh, rh) + 1
        }
    }

  def height2[A](tree: BinaryTree[A]): Int =
    tree match {
      case Empty                => 0
      case Node(_, left, right) => 1 + max(height2(left), height2(right))
    }
}
