import BinaryTree.{Empty, Node, TraversalOrder}
import cats.Show
import cats.data.Chain

import Math._
import cats.syntax.all._

sealed trait BinaryTree[+A] {
  def fold[B](emptyVal: B)(mergeFn: (B, A, B) => B): B = this match {
    case BinaryTree.Empty => emptyVal
    case Node(value, left, right) =>
      mergeFn(left.fold(emptyVal)(mergeFn), value, right.fold(emptyVal)(mergeFn))
  }

  def foldNLR[B](zero: B)(fe: B => B, fn: (B, Node[A]) => B): B = this match {
    case Empty => fe(zero)
    case n @ Node(_, left, right) =>
      val nres = fn(zero, n)
      val lres = left.foldNLR(nres)(fe, fn)
      right.foldNLR(lres)(fe, fn)
  }

  def foldLNR[B](zero: B)(fe: B => B, fn: (B, Node[A]) => B): B = this match {
    case Empty => fe(zero)
    case n @ Node(_, left, right) =>
      val lres = left.foldLNR(zero)(fe, fn)
      val nres = fn(lres, n)
      right.foldLNR(nres)(fe, fn)
  }

  def foldLRN[B](zero: B)(fe: B => B, fn: (B, Node[A]) => B): B = this match {
    case Empty => fe(zero)
    case n @ Node(_, left, right) =>
      val lres = left.foldLRN(zero)(fe, fn)
      val rres = right.foldLRN(lres)(fe, fn)
      fn(rres, n)
  }

  def foldNRL[B](zero: B)(fe: B => B, fn: (B, Node[A]) => B): B = this match {
    case Empty => fe(zero)
    case n @ Node(_, left, right) =>
      val nres = fn(zero, n)
      val rres = right.foldNRL(nres)(fe, fn)
      left.foldNRL(rres)(fe, fn)
  }

  def foldRNL[B](zero: B)(fe: B => B, fn: (B, Node[A]) => B): B = this match {
    case Empty => fe(zero)
    case n @ Node(_, left, right) =>
      val rres = right.foldRNL(zero)(fe, fn)
      val nres = fn(rres, n)
      left.foldRNL(nres)(fe, fn)
  }

  def foldRLN[B](zero: B)(fe: B => B, fn: (B, Node[A]) => B): B = this match {
    case Empty => fe(zero)
    case n @ Node(_, left, right) =>
      val rres = right.foldRLN(zero)(fe, fn)
      val lres = left.foldRLN(rres)(fe, fn)
      fn(lres, n)
  }

  def foldValuesNLR[B](zero: B)(fn: (B, A) => B): B = foldNLR(zero)(identity, (b, n) => fn(b, n.value))
  def foldValuesLNR[B](zero: B)(fn: (B, A) => B): B = foldLNR(zero)(identity, (b, n) => fn(b, n.value))
  def foldValuesLRN[B](zero: B)(fn: (B, A) => B): B = foldLRN(zero)(identity, (b, n) => fn(b, n.value))
  def foldValuesNRL[B](zero: B)(fn: (B, A) => B): B = foldNRL(zero)(identity, (b, n) => fn(b, n.value))
  def foldValuesRNL[B](zero: B)(fn: (B, A) => B): B = foldRNL(zero)(identity, (b, n) => fn(b, n.value))
  def foldValuesRLN[B](zero: B)(fn: (B, A) => B): B = foldRLN(zero)(identity, (b, n) => fn(b, n.value))

  def foldDepthFirst[B](order: TraversalOrder, zero: B)(fe: B => B, fn: (B, Node[A]) => B): B =
    order match {
      case TraversalOrder.NLR => foldNLR(zero)(fe, fn)
      case TraversalOrder.LNR => foldLNR(zero)(fe, fn)
      case TraversalOrder.LRN => foldLRN(zero)(fe, fn)
      case TraversalOrder.NRL => foldNRL(zero)(fe, fn)
      case TraversalOrder.RNL => foldRNL(zero)(fe, fn)
      case TraversalOrder.RLN => foldRLN(zero)(fe, fn)
    }

  def foldValuesDepthFirst[B](order: TraversalOrder, zero: B)(fn: (B, A) => B): B =
    foldDepthFirst[B](order, zero)(identity, (b, n) => fn(b, n.value))

  def mapNode[B](fn: Node[A] => B): BinaryTree[B] =
    this match {
      case Empty                    => Empty
      case n @ Node(_, left, right) => Node[B](fn(n), left.mapNode(fn), right.mapNode(fn))
    }

  def map[B](fn: A => B): BinaryTree[B] = mapNode(n => fn(n.value))

  def trim(fn: A => Boolean): BinaryTree[A] = this match {
    case Empty => Empty
    case Node(value, left, right) =>
      if (fn(value)) Node(value, left.trim(fn), right.trim(fn))
      else Empty
  }

  def trimMap[B](fn: A => Option[B]): BinaryTree[B] = this match {
    case Empty => Empty
    case Node(value, left, right) =>
      fn(value) match {
        case None        => Empty
        case Some(value) => Node(value, left.trimMap(fn), right.trimMap(fn))
      }
  }

  def zip[B](other: BinaryTree[B]): BinaryTree[(A, B)] =
    (this, other) match {
      case (Node(a, la, ra), Node(b, lb, rb)) => Node(a -> b, la.zip(lb), ra.zip(rb))
      case _                                  => Empty
    }

  def zipAll[B](other: BinaryTree[B]): BinaryTree[(Option[A], Option[B])] =
    (this, other) match {
      case (Empty, Empty) => Empty
      case (Node(value, left, right), Empty) =>
        Node(value.some -> none[B], left.zipAll(Empty), right.zipAll(Empty))
      case (Empty, Node(value, left, right)) =>
        Node(none[A] -> value.some, Empty.zipAll(left), Empty.zipAll(right))
      case (Node(a, la, ra), Node(b, lb, rb)) =>
        Node(a.some -> b.some, la.zipAll(lb), ra.zipAll(rb))
    }
}

object BinaryTree {
  case object Empty extends BinaryTree[Nothing]

  case class Node[+A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]
  object Node {
    def apply[A](value: A): Node[A] = new Node(value, Empty, Empty)

    def apply[A](value: A, left: A, right: A): Node[A] =
      new Node(value, Node(left), Node(right))

    def left[A](value: A, left: A): Node[A] =
      new Node[A](value, Node(left), Empty)

    def right[A](value: A, right: A): Node[A] =
      new Node(value, Empty, Node(right))

    def right[A](value: A, right: Node[A]): Node[A] =
      new Node(value, Empty, right)
  }

  sealed abstract class TraversalOrder
  object TraversalOrder {
    case object NLR extends TraversalOrder
    case object LNR extends TraversalOrder
    case object LRN extends TraversalOrder
    case object NRL extends TraversalOrder
    case object RNL extends TraversalOrder
    case object RLN extends TraversalOrder
  }

  object IsLeaf {
    def unapply[A](t: BinaryTree[A]): Option[Node[A]] =
      t match {
        case Node(value, Empty, Empty) => Node(value, Empty, Empty).some
        case _                         => None
      }
  }

  def traverseInOrder[A](root: BinaryTree[A], fn: Node[A] => Unit): Unit =
    root match {
      case Empty                     => ()
      case n @ Node(_, Empty, Empty) => fn(n)
      case n @ Node(_, left, right) =>
        traverseInOrder[A](left, fn)
        fn(n)
        traverseInOrder[A](right, fn)
    }

  def isBalanced0[A](root: BinaryTree[A]): Boolean = {
    def loop[A](root: BinaryTree[A]): Int =
      root match {
        case Empty => -1
        case Node(_, left, right) =>
          val lh = loop(left)
          if (lh == -2) -2
          else {
            val rh = loop(right)
            if (rh == -2) -2
            else if (abs(lh - rh) > 1) -2
            else max(lh, rh) + 1
          }
      }

    loop(root) != -2
  }

  def isBalanced1[A](root: BinaryTree[A]): Boolean =
    -2 =!= root.fold(-1) { (l, _, r) =>
      if (l == -2) -2
      else if (r == -2) -2
      else if (abs(l - r) > 1) -2
      else max(l, r) + 1
    }

  def height0[A](tree: BinaryTree[A]): Int =
    tree match {
      case Empty                => 0
      case Node(_, left, right) => 1 + max(height0(left), height0(right))
    }

  def height1[A](tree: BinaryTree[A]): Int = tree.fold(0)((l, _, r) => 1 + max(l, r))

  object builderSyntax {
    implicit final class ValueLifts[A](private val a: A) extends AnyVal {
      def leaf: BinaryTree[A]                                       = Node(a, Empty, Empty)
      def branch(l: BinaryTree[A], r: BinaryTree[A]): BinaryTree[A] = Node(a, l, r)
      def left(ln: BinaryTree[A]): BinaryTree[A]                    = Node(a, ln, Empty)
      def right(rn: BinaryTree[A]): BinaryTree[A]                   = Node(a, Empty, rn)
    }
  }

  def render0[A: Show](tree: BinaryTree[A]): String = {
    val Column  = "| "
    val Spacer  = "  "
    val builder = new StringBuilder
    def loop(node: BinaryTree[A], prefix: Chain[String], lastPrefix: String, lastChild: Boolean): Unit =
      node match {
        case Empty => ()
        case Node(value, left, right) =>
          val hasChildren = left != Empty || right != Empty
          prefix.iterator.foreach(builder.append)
          builder
            .append(if (lastChild) "'" else "+")
            .append("-")
            .append(if (hasChildren) ':' else '-')
            .append(' ')
            .append(value.show)
            .append('\n')
          loop(left, prefix.append(lastPrefix), Column, lastChild = right == Empty)
          loop(right, prefix.append(lastPrefix), Spacer, lastChild = true)
      }

    tree match {
      case Empty =>
        builder.append("Empty").append('\n')
      case Node(value, left, right) =>
        builder.append(": ").append(value.show).append('\n')
        loop(left, Chain.empty, Column, lastChild = right == Empty)
        loop(right, Chain.empty, Spacer, lastChild = true)
    }
    builder.result()
  }

}
