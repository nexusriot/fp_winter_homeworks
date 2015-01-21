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
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Функция создает множество, состоящее из одного элемента
   */
  def singletonSet(elem: Int): Set = set => set == elem

  /**
   * Объединение множеств, в результирующее множество
   * входят элементы из `s` или `t`
   */
  def union(s: Set, t: Set): Set = {elem: Int => contains(s, elem) || contains(t, elem)}

  /**
   * Пересечение множеств, только общиее для `s` или `t` элементы
   */
  def intersect(s: Set, t: Set): Set = {elem: Int => contains(s, elem) && contains(t, elem)}

  /**
   * Вычитание, элементы из `s`, которых нет в `t`
   * the set of all elements of `s` that are not in `t`.
   */
  def sub(s: Set, t: Set): Set = {elem: Int => contains(s, elem) && !contains(t, elem)}

  /**
   * Элементы `s`, удовлетворяющие предикату `p`.
   */
  def filter(s: Set, p: Int => Boolean): Set = {elem : Int => contains(s, elem) && contains (p, elem)} ///???????

  /**
   * Удовлетворяют ли все целые числа в отрезке [-bound; bound] из `s` предикату `p`.
   */
  def forall(s: Set, p: Int => Boolean, bound: Int): Boolean = {
    def loop(i: Int): Boolean = {
      if (-bound == i) true else if ((contains(s, i)) && (!contains(filter(s,p), i))) false
      else loop(i-1)
      }
    loop(bound)
  }

  /**
   * Существует ли в `s` целое в отрезке [-bound; bound],
   * удовлетворяющее `p`.
   */
  def exists(s: Set, p: Int => Boolean, bound: Int): Boolean = {
    def loop(i: Int): Boolean = {
      if (-bound == i) false else if ((contains(s, i)) && (contains(filter(s,p), i))) true
      else loop(i-1)
    }
    loop(bound)
  }

  /**
   * Применяет `f` к каждому элементу `s` в отрезке [-bound; bound].
   */
  def map(s: Set, f: Int => Int, bound: Int): Set =  {
    def loop(base: Set, modified: Set, f: Int => Int, elem: Int): Set = {
      if (elem > bound) modified
      else if (contains(base, elem)) loop(base, union(modified, singletonSet(f(elem))), f, elem + 1)
      else loop(base, modified, f, elem + 1)
    }
    loop(s, (elem: Int) => false, f,-bound)
  }


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
