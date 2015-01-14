package sets

object IntSet {
  /*
   *  Мы задаем множество характерестической функцией.
   *  Элементы, для которых предикат True, содержаться в множестве,
   *  False - нет
   */
  type Set = Int => Boolean

  /**
   * Проверка, входит ли элемент в множество
   */
  def contains(s: Set, elem: Int): Boolean = ???

  /**
   * Функция создает множество, состоящее из одного элемента
   */
  def singletonSet(elem: Int): Set = ???

  /**
   * Объединение множеств, в результирующее множество
   * входят элементы из `s` или `t`
   */
  def union(s: Set, t: Set): Set = ???

  /**
   * Пересечение множеств, только общиее для `s` или `t` элементы
   */
  def intersect(s: Set, t: Set): Set = ???

  /**
   * Вычитание, элементы из `s`, которых нет в `t`
   * the set of all elements of `s` that are not in `t`.
   */
  def sub(s: Set, t: Set): Set = ???

  /**
   * Элементы `s`, удовлетворяющие предикату `p`.
   */
  def filter(s: Set, p: Int => Boolean): Set = ???

  /**
   * Удовлетворяют ли все целые числа в отрезке [-bound; bound] из `s` предикату `p`.
   */
  def forall(s: Set, p: Int => Boolean, bound: Int): Boolean = ???

  /**
   * Существует ли в `s` целое в отрезке [0; bound],
   * удовлетворяющее `p`.
   */
  def exists(s: Set, p: Int => Boolean, bound: Int): Boolean = ???

  /**
   * Применяет `f` к каждому элементу `s` в отрезке [-bound; bound].
   */
  def map(s: Set, f: Int => Int, bound: Int): Set = ???


  /**
   * Displays the contents of a set
   */
  def toString(s: Set, bound: Int): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s, 1000))
  }
}
