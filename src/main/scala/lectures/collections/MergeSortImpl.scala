package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    def merge(left: Seq[Int], right: Seq[Int]): Seq[Int] = (left, right) match {
      case (Nil, _) => right
      case (_, Nil) => left
      case _ =>
        if (left.head < right.head)
          left.head +: merge(left.tail, right)
        else
          right.head +: merge(left, right.tail)
    }

    val half = data.length / 2
    val parts = data.splitAt(half)
    if (half == 1)
      merge(parts._1, parts._2)
    else
      merge(mergeSort(parts._1), mergeSort(parts._2))
  }

  val unsorted = Seq(1, 8, 5, 6, 1, 7, 8, 12, 42, 51, 12, 12, 123, 11, 225, 1, 3, 5, 2, 3, 4)
  println(mergeSort(unsorted))
}
