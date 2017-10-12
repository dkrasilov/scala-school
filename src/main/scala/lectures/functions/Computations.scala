package lectures.functions

/**
  *
  * В объекте 'Computation' в методе computation сравниваются 2 массива.
  * Результатом сравнения будет массив, содержащий слова, принадлежащие обоим массивам
  * В данном случа результатом будет массив, содержащий 2 элемента Array("Клара", "Карла")
  *
  * С помощью Thread.sleep имитируется прододжительное вычисление
  */
trait Data {
  val filterData = "Клара у Карла украла корралы, Карл у Клары украл кларнет"
  val dataArray = "Клара Цеткин обожала Карла Маркса".split(" ")
}

object Computation extends App with Data {

  def computation(filterData: String, dataProducer: Array[String]): Array[String] =
    dataProducer.filter(dataItem => filterData.split(" ").contains(dataItem))

  val result = computation(filterData, dataArray)
  result.foreach(println)
}

/**
  * Допишите curriedComputation, так, что бы после вызова partiallyAppliedCurriedFunction
  * результат был бы тем же, что и в предыдущем случае
  *
  * Раскомментируйте последнюю строчку
  *
  * Какой тип имеет partiallyAppliedCurriedFunction - ?
  */
object CurriedComputation extends App with Data {

  def curriedComputation(filterData: String)(dataProducer: Array[String]): Array[String] =
    dataProducer.filter(filterData.split(" ").contains(_))

  val partiallyAppliedCurriedFunction = curriedComputation(filterData = filterData) _

  val result = partiallyAppliedCurriedFunction(dataArray)
  result.foreach(println)
}

/**
  * Допишите реализации методов так, что бы результат совпадал с предыдущими.
  *
  * При этом постарайтесь минимизировать количество разбиений строки filterData на отдельные слова.
  */
object FunctionalComputation extends App with Data {

  def functionalComputation(filterData: String): (Array[String]) => Array[String] =
    (dataArray) => dataArray.filter(filterData.split(" ").contains(_))

  val filterApplied = functionalComputation(filterData)

  val result = filterApplied(dataArray)
  result.foreach(println)
}
