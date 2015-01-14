package sets

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IntSetTest extends FunSuite {


  /**
   * Ссылка на scaladoc - отлиное руководство FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Операторы
   *  - test
   *  - ignore
   *  - pending
   */

  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * Для тестов ScalaTest, существует оператор  "===", который может быть
   * использован в "assert". Если утверждение не верно, оба значения будут
   * распечатаны в сообщении об ошибке. Когда используется обычный "==",
   * сообщение об ошибке будет выглядеть так: "assertion failed", без
   * дополнительной информации.
   *
   * Попробуйте! Измените опреатор и посмотрите на результат
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  import IntSet._

  test("contains реализован") {
    assert(contains(x => true, 100))
  }

  /**
   * Во время создания тестов мы можем захотеть использовать какие-то значения в
   * нескольких тестах. В данном случае мы хотим использовать IntSet во всех тестах.
   *
   * Вместо копирования кода создания мы создадим множества в тестовом классе с
   * помощью val:
   *
   *   val s1 = singletonSet(1)
   *
   * Что случиться, если "singletonSet" будет иметь ошибку упадет? Ни один тест не
   * выполниться, т.к. тестовый класс не создастся!
   *
   * Поэтому мы вынесли создание объектов в trait (похож на абстрактный класс) и
   * создаем его объект в каждом тестовом методе
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val bound = 1000
    def test_forall: (Set, Int => Boolean) => Boolean = forall(_, _, bound)
    def test_exists: (Set, Int => Boolean) => Boolean = exists(_, _, bound)
    def test_map: (Set, Int => Int) => Set = map(_, _, bound)
  }

  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union включает все элементы") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect включает общие элементы") {
    new TestSets {
      val testSet1 = union(s1, s2)
      val testSet2 = union(s3, s2)
      assert(contains(intersect(testSet1, testSet2), 2), "Intersect (1,2) & (2,3)")
    }
  }

  test("diff включает различные элементы") {
    new TestSets {
      val testSet1 = union(s1, s2)
      val testSet2 = union(s3, s2)
      assert(contains(sub(testSet1, testSet2), 1), "diff (1,2) & (2,3) включает 1")
      //assert(contains(diff(testSet1, testSet2), 3), "diff (1,2) & (2,3) contains 3")
    }
  }

  test("filter включает элементы, удовлетворяющие заданному предикату") {
    new TestSets {
      val s = union(s3,  union(s1, s2))
      assert(contains(filter(s, (e: Int) => e < 3), 1), "отфильтрованное множество включает 1")
      assert(contains(filter(s, (e: Int) => e < 3), 2), "отфильтрованное множество включает 2")
      assert(!contains(filter(s, (e: Int) => e < 3), 3), "отфильтрованное множество включает 3")
    }
  }

  test("forall должен возвращать true если все элементы удовлетворяют заданному предекату и false иначе") {
    new TestSets {
      val s = union(s3,  union(s1, s2))
      assert(test_forall(s, (e: Int) => e < 4), "все целые < 4")
      assert(!test_forall(s, (e: Int) => e < 3), "не все целые < 3")
    }
  }

  test("exist должен возвращать true если во множестве есть элемент, удовлетворяющий предекату, иначе false") {
    new TestSets {
      val s = union(s3,  union(s1, s2))
      assert(test_exists(s, (e: Int) => e == 3), "целое 3 существует")
      assert(!test_exists(s, (e: Int) => e > 4), "нет целых > 4")
    }
  }

  test("map должен вернуть множество, в котором каждый элемент `f` - это трансформированная версия `s`") {
    new TestSets {
      val s = union(s3,  union(s1, s2))
      def f =  (e: Int) => e + 3
      assert(contains(test_map(s, f), 4), "4 существует")
      assert(contains(test_map(s, f), 5), "5 существует")
      assert(contains(test_map(s, f), 6), "6 существует")
      assert(!contains(test_map(s, f), 7), "7 не существует")
    }
  }

}