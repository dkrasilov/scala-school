package lectures.oop

import org.jboss.netty.handler.codec.http.HttpChunkAggregator


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения (null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl:
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужно раскомментировать и реализовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор:
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]

  def fold(aggregator: Int)(f: (Int, Int) => Int): Int
}

case class BSTImpl(value: Int,
                   left: Option[BST] = None,
                   right: Option[BST] = None) extends BST {

  def add(newValue: Int): BST =
    if (newValue > value)
      right match {
        case Some(r) => BSTImpl(value, left, Some(r.add(newValue)))
        case None => BSTImpl(value, left, Some(BSTImpl(newValue)))
      }
    else if (newValue < value)
      left match {
        case Some(l) => BSTImpl(value, Some(l.add(newValue)), right)
        case None => BSTImpl(value, Some(BSTImpl(newValue)), right)
      } else this

  def find(value: Int): Option[BST] =
    if (value > this.value) right match {
      case Some(rightTree) => rightTree.find(value)
      case None => None
    } else if (value < this.value) left match {
      case Some(leftTree) => leftTree.find(value)
      case None => None
    } else Some(this)

  def fold(aggregator: Int)(f: (Int, Int) => Int): Int = (left, right) match {
    case (None, None) => f(aggregator, value)
    case (Some(l: BSTImpl), None) => l.fold(f(aggregator, value))(f)
    case (None, Some(r: BSTImpl)) => r.fold(f(aggregator, value))(f)
    case (Some(l: BSTImpl), Some(r: BSTImpl)) => r.fold(l.fold(f(aggregator, value))(f))(f)
  }

  override def toString: String = "(" + left.getOrElse(".") + value + right.getOrElse(".") + ")"

}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = (1 to nodesCount).foldLeft(root)(_ add _) // generator goes here

  println(tree)
  println(tree.fold(0)(_ + _))

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)

  println(testTree)
}