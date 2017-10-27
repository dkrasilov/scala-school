package lectures.collections

import lectures.collections.CherryTree.{Node, Node1, Node2}

import scala.collection.generic._
import scala.collection.{GenTraversableOnce, LinearSeq, LinearSeqOptimized, mutable}

sealed trait CherryTree[+T] extends LinearSeq[T] with LinearSeqOptimized[T, CherryTree[T]]
  with GenericTraversableTemplate[T, CherryTree] with Product with Serializable {

  override def init: CherryTree[T] = super.init
  override def last: T = super.last

  override def apply(n: Int): T
  def append[S >: T](x: S): CherryTree[S]

  def prepend[S >: T](x: S): CherryTree[S]

  def concat[S >: T](xs: CherryTree[S]): CherryTree[S] = ???

  override def toString(): String = super.toString()

  override def companion = CherryTree

  override def stringPrefix: String = "CherryTree"


  // If we have a default builder, there are faster ways to perform some operations
  @inline private[this] def isDefaultCBF[A, B, That](bf: CanBuildFrom[CherryTree[A], B, That]): Boolean = bf eq CherryTree.ReusableCBF

  override def :+[B >: T, That](elem: B)(implicit bf: CanBuildFrom[CherryTree[T], B, That]) =
    if (isDefaultCBF(bf)) append(elem).asInstanceOf[That] else super.:+(elem)

  override def +:[B >: T, That](elem: B)(implicit bf: CanBuildFrom[CherryTree[T], B, That]) =
    if (isDefaultCBF(bf)) prepend(elem).asInstanceOf[That] else super.:+(elem)

  override def ++[B >: T, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[CherryTree[T], B, That]) =
    if (isDefaultCBF(bf)) concat(that.asInstanceOf[CherryTree[B]]).asInstanceOf[That] else super.++(that)
}

case object CherryNil extends CherryTree[Nothing] {
  override def head = throw new NoSuchElementException("head of empty CherryList")
  override def tail = throw new UnsupportedOperationException("tail of empty CherryList")

  override def init = throw new UnsupportedOperationException("init of empty CherryList")

  override def last = throw new NoSuchElementException("last of empty CherryList")

  override def apply(n: Int): Nothing = throw new NoSuchElementException("cannot apply empty CherryList")

  override def foreach[U](f: (Nothing) => U): Unit = ()

  override def append[S >: Nothing](x: S): CherryTree[S] = CherrySingle(x)

  override def prepend[S >: Nothing](x: S): CherryTree[S] = CherrySingle(x)

  override def concat[S >: Nothing](xs: CherryTree[S]): CherryTree[S] = xs

  override def size = 0

  override def isEmpty = true
}

final case class CherrySingle[+T](x: T) extends CherryTree[T] {
  override def head: T = x
  override def tail: CherryNil.type = CherryNil


  override def init: CherryNil.type = CherryNil

  override def last: T = x

  override def apply(n: Int): T =
    if (n == 0) x
    else throw new NoSuchElementException("no element with index " + n + "in singleton tree")

  override def foreach[U](f: T => U): Unit = f(x)

  def append[S >: T](y: S) = CherryBranch(Node1(x), CherryNil, Node1(y))

  override def prepend[S >: T](y: S): CherryTree[S] = CherryBranch(Node1(y), CherryNil, Node1(x))

  override def concat[S >: T](xs: CherryTree[S]): CherryTree[S] = xs.prepend(x)

  override def size = 1

  override def isEmpty = false
}

final case class CherryBranch[+T](left: Node[T], inner: CherryTree[Node2[T]], right: Node[T]) extends CherryTree[T] {
  override def head: T = left match {
    case Node1(x) => x
    case Node2(x, _) => x
  }

  override def tail: CherryTree[T] = left match {
    case Node1(_) => inner match {
      case CherryNil => right match {
        case Node1(x) => CherrySingle(x)
        case Node2(x, y) => CherryBranch(Node1(x), CherryNil, Node1(y))
      }
      case tree => CherryBranch(tree.head, tree.tail, right)
    }
    case Node2(_, x) => CherryBranch(Node1(x), inner, right)
  }

  override def init: CherryTree[T] = right match {
    case Node1(_) => inner match {
      case CherryNil => left match {
        case Node1(x) => CherrySingle(x)
        case Node2(x, y) => CherryBranch(Node1(x), CherryNil, Node1(y))
      }
      case tree => CherryBranch(left, tree.init, tree.last)
    }
    case Node2(x, _) => CherryBranch(left, inner, Node1(x))
  }

  override def last: T = right match {
    case Node1(x) => x
    case Node2(_, y) => y
  }

//  override def apply(n: Int): T = (n, size) match {
//    case _ if n > size => throw new IndexOutOfBoundsException("no element with index " + n)
//    case (1, _) => left match {
//      case Node1(x) => x
//      case Node2(x, _) => x
//    }
//    case (2, _) => left match {
//      case Node1(_) => inner.head.x
//      case Node2(_, y) => y
//    }
//    case _ if n == size - 1 => right match {
//      case Node1(x) => x
//      case Node2(_, y) => y
//    }
//    case _ if n == size - 2 => right match {
//      case Node1(x) => inner.last.y
//      case Node2(x, _) => x
//    }
//    case _ => inner.app
//  }

  override def foreach[U](f: T => U): Unit = {
    left.foreach(f)
    inner.foreach(_.foreach(f))
    right.foreach(f)
  }

  override def prepend[S >: T](elem: S): CherryBranch[S] = left match {
    case Node1(x) => CherryBranch(Node2(elem, x), inner, right)
    case n: Node2[S] => CherryBranch(Node1(elem), inner.prepend(n), right)
  }

  override def append[S >: T](x: S): CherryBranch[S] = right match {
    case Node1(y) => CherryBranch(left, inner, Node2(y, x))
    case n: Node2[S] => CherryBranch(left, inner.append(n), Node1(x))
  }


  override def concat[S >: T](xs: CherryTree[S]): CherryBranch[S] = xs match {
    case CherryNil => this
    case CherrySingle(x) => this.append(x)
    case CherryBranch(left, inner, right) =>
  }

  override def size: Int = left.size + inner.size * 2 + right.size

  override def isEmpty = false
}


object CherryTree extends SeqFactory[CherryTree] {

  private class CherryTreeBuilder[T]() extends mutable.Builder[T, CherryTree[T]] {
    private[this] var coll: CherryTree[T] = CherryNil

    def +=(elem: T) = {
      coll = coll.append(elem)
      this
    }

    def clear(): Unit = coll = CherryNil

    def result(): CherryTree[T] = coll
  }

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, CherryTree[A]] =
    ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  def newBuilder[T]: mutable.Builder[T, CherryTree[T]] = new CherryTreeBuilder[T]

  sealed trait Node[+T] {
    def foreach[U](f: T => U): Unit
    def size: Int
  }

  final case class Node1[+T](x: T) extends Node[T] {
    override def foreach[U](f: (T) => U): Unit = f(x)
    def size = 1
  }

  final case class Node2[+T](x: T, y: T) extends Node[T] {
    def foreach[U](f: (T) => U): Unit = {
      f(x)
      f(y)
    }
    def size = 2
  }
}

object test extends App {
  val tree = CherryTree(1, 2, 3, 4, 5)
}