package lectures.collections.comprehension

/**
  * Помогите курьерам разобраться с обслуживанием адресов
  *
  * Каждый день на работу выходит 'courierCount' курьеров
  * Им нужно обслужить 'addressesCount' адресов
  * Каждый курьер может обслужить courier.canServe адресов, но только при условии, что позволит дорожная ситуация.
  * Т.е. если trafficDegree < 5, то курьер обслужит все адреса, которые может, иначе - ни одного
  *
  * Входные данные для приложения содержат 2 строки
  * В первой строке - количество адресов, которые требуется обслужить
  * Во второй - количество курьеров, вышедших на работу.
  *
  * Ваша задача:
  *  Изучить код и переписать его так,
  *  что бы в нем не было ни одного цикла for, ни одной переменной или мутабильной коллекции
  *
  * Для этого используйте функции комбинаторы: filter, withFilter, fold, map, flatMap и т.д.
  *
  */

case class Traffic(degree: Double)

object Courier {
  def couriers(courierCount: Int): List[Courier] = (courierCount to 1 by -1).foldLeft(List[Courier]())((list, num) => Courier(num) :: list)
}

case class Courier(index: Int) {
  val canServe: Int = (Math.random() * 10).toInt

  override def toString = "Courier(index:" + index + ", canServe:" + canServe + ")"
}

object Address {
  def addresses(addressesCount: Int): List[Address] = (1 to addressesCount).map(x => Address(s"$x$x$x")).toList
}

case class Address(postIndex: String)

object CouriersWithComprehension extends App {

  import Address._
  import Courier._

  val sc = new java.util.Scanner(System.in)
  val addressesCount = sc.nextInt()
  val courierCount = sc.nextInt()
  val addrs = addresses(addressesCount)
  println("Adresses: " + addrs)
  val cours = couriers(courierCount)
  println("Couriers: " + cours)

  // какие адреса были обслужены
  def serveAddresses(addresses: List[Address], couriers: List[Courier]): List[Address] =
    addresses.take(
      couriers.filter(_ => traffic().degree < 5).foldLeft(0)(_ + _.canServe)
    )

  // old version
  //  {
  //    var accum = 0
  //    for (courier <- couriers;
  //         trafficDegree = traffic().degree;
  //         t <- 0 until courier.canServe if trafficDegree < 5 && accum < addresses.length
  //    ) yield {
  //      val addr = addresses(accum)
  //      accum = accum + 1
  //      addr
  //    }
  //  }

  def traffic(): Traffic = Traffic(Math.random() * 10)

  def printServedAddresses(addresses: List[Address], couriers: List[Courier]): Unit =
    serveAddresses(addresses, couriers).foreach(x => println(x.postIndex))

  printServedAddresses(addrs, cours)

}
