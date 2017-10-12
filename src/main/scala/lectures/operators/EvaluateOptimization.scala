package lectures.operators

import lectures.functions.{Computation, CurriedComputation, Data, FunctionalComputation}

/**
  * В задачке из lectures.functions.Computations мы реализовали
  * один и тот же метод 3-мя разными способами
  *
  * Пришло время оценить, насколько разные имплементации
  * отличаются друг от друга по производительности
  *
  * Для этого
  *   * в классах CurriedComputation и FunctionalComputation уберите extends App, оставьте extends Data
  *   * раскомментируйте код, выполните в циклах вызов 3-х имплементаций,
  *   * оцените разницу во времени выполнения и объясните ее происхожение
  *
  */
object EvaluateOptimization extends App with Data {

  val computationStartTimestamp = System.currentTimeMillis()

  val range = 1 to 100
//  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 Computation.computation
  for (_ <- range) {
    Computation.computation(filterData, dataArray)
  }

  println("Elapsed time in computation(): " + (System.currentTimeMillis() - computationStartTimestamp))



  val partiallyAppliedStartTimestamp = System.currentTimeMillis()

  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 CurriedComputation.partiallyAppliedCurriedFunction
  for (_ <- range) {
    CurriedComputation.curriedComputation(filterData)(dataArray)
  }

  val partiallyAppliedDuration = System.currentTimeMillis() - partiallyAppliedStartTimestamp
  println("Elapsed time in partiallyAppliedCurriedFunction(): " + partiallyAppliedDuration)



  val filterAppliedStartTimestamp = System.currentTimeMillis()

  // ВЫПОЛНИТЬ В ЦИКЛЕ ОТ 1 ДО 100 FunctionalComputation.filterApplied
  for (_ <- range) {
    FunctionalComputation.functionalComputation(filterData)(dataArray)
  }

  val filterAppliedDuration = System.currentTimeMillis() - filterAppliedStartTimestamp
  println("Elapsed time in filterApplied():" + filterAppliedDuration)

  // ВЫВЕСТИ РАЗНИЦУ В ПРОДОЛЖИТЕЛЬНОСТИ ВЫПОЛНЕНИЯ МЕЖДУ КАРРИРОВАННОЙ ВЕРСИЕЙ
  // И ФУНКЦИОНАЛЬНОЙ

  val diff = filterAppliedDuration - partiallyAppliedDuration

  println(s"Difference is about $diff milliseconds")
}

