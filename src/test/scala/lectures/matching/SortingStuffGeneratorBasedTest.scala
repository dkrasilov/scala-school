package lectures.matching

import lectures.matching.SortingStuff.{Book, Knife, StuffBox, Watches, Boots}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random


/**
  * Короткий список самых востребованных генераторов:
  * Gen.alphaString
  * Gen.delay
  * Gen.oneOf
  * Gen.resultOf
  * Gen.zip
  * Gen.map
  * Gen.suchThat
  * Gen.mapOf
  * Gen.pic
  * Gen.choose
  *
  * Допишите 2 теста:
  * Для "find knife" теста создайте генератор, Option[Knife]. Тест должен показать, что если нож есть в вещах,
  * то метод findMyKnife его отыщет.
  *
  * Для "put boots ..." создайте генератор и проверьте правильность работы метода sortJunk по аналогии с предыдущими тестами.
  *
  */

class SortingStuffGeneratorBasedTest extends WordSpec with Matchers with PropertyChecks {

  val cheepWatchGen: Gen[Watches] = Gen.zip(Gen.choose(0f, 1000f), Gen.alphaStr).map(w => Watches(w._2, w._1))
  val bookGenerator = Gen.alphaStr.map(name => Book(name, Random.nextBoolean()))
  val interestingBookGen = bookGenerator.filter(_.isInteresting)
  val knifeGenerator = Gen.option(Knife)
  val bootsGenerator = Gen.oneOf("Adidas", "Converse", "Noname").map(brand => Boots(brand))
  val coolBootsGen = bootsGenerator.filter(boot => List("Adidas", "Converse") contains boot.brand)

  // Override configuration if you need
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(minSize = 10, maxSize = 20)

  val get: AfterWord = new AfterWord("have")

  "This test" should get {
    "proper cheep watch generator" in {
      forAll(cheepWatchGen) { (watch: Watches) => {
        println(watch)
        watch.cost should be <= 1000f
      }
      }
    }
    "proper interesting book generator" in {
      val books = interestingBookGen
      forAll(books) { (book: Book) => {
        println(book)
        book shouldBe 'interesting
      }
      }
    }
    "proper cool boots generator" in {
      val boots = coolBootsGen
      forAll(boots) { (boot: Boots) => {
        println(boot)
        boot.brand should fullyMatch regex "(Adidas|Converse)"
      }
      }
    }


    "proper knife generator" in {
      val knifes = knifeGenerator
      forAll(knifes) { (knife: Option[Knife.type]) =>
        println(knife)
      }
    }
  }

  "Sort stuff" should {
    "return collections" which {
      "total size is equal to item amount" in {
        val ms = generatorDrivenConfig.minSuccessful

        val books = (1 to ms) flatMap { _ => interestingBookGen.sample }
        val watches = (1 to ms) flatMap { _ => cheepWatchGen.sample }

        val StuffBox(goodBooks, niceWatches, _, junk) = SortingStuff.sortJunk(Random.shuffle(books ++ watches).toList)
        goodBooks should have size books.size
        niceWatches should have size 0
        junk should have size watches.size
      }
    }
    "find knife" which {
      "was occasionally disposed" in {
        val knifes = knifeGenerator
        forAll(knifes) { (knife: Option[Knife.type]) =>
          if (knife.isEmpty)
            SortingStuff.findMyKnife(SortingStuff.sortJunk(List())) shouldBe false
          else
            SortingStuff.findMyKnife(SortingStuff.sortJunk(List(knife.get))) shouldBe true
        }
      }
    }

    "put boots in a proper place" when {
      "boots were produced by Converse or Adidas" in {
        val boots = (1 to generatorDrivenConfig.minSize) flatMap (_ => coolBootsGen.sample)
        val StuffBox(_, _, coolBoots, _) = SortingStuff.sortJunk(boots.toList)

        coolBoots should have size boots.size
      }
    }
  }
}
