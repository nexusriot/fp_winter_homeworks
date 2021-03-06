package exercise1

object Exercise1 {
  /**
   * Exercise 1
   *
   * Теугольник Паскаля - это треугольник, в котором по краям стоят 1, а все остальные
   * элементы равны сумме двух вышестоящих:
   *
   *        1
   *       1 1
   *      1 2 1
   *     1 3 3 1
   *    1 4 6 4 1
   *    .........
   *
   * Эта функция производит расчет элемента треугольника Паскаля в строке `r` и горизонтальной позиции `c`.
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   *
   * Функция проверки балансировки скобок. Если в строке каждой открывающейся скобке соответсвует закрывающееся
   * (например (), (sdsadasd), (dsd)(sd)sdsdsd()) то результат true, иначе false (%-), )( и т.д.).
   */
  def balance(chars: List[Char]): Boolean = {

    def f(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else if (chars.head == '(') f(chars.tail,open + 1)
      else if (chars.head == ')') open > 0 && f(chars.tail,open - 1)
      else f(chars.tail, open)
    }
    f(chars, 0)
  }


  /**
   * Exercise 3
   *
   * Размен заданной суммы задаными монетами.
   *
   * Функция определяет, сколькими способами можно разменять сумму `money` монетами из списка `coins`
   * Например 4 можно разменять 3 способами монетами 1 и 2: 1+1+1+1, 1+1+2, 2+2.
   *
   * В работе могут прегодиться следующие методы класса List: isEmpty, head и tail
   *
   * Совет: начните рассуждать с вырожденного случая когда нужно разменять нулевую сумму,
   * когда нужно разменять сумму 0 монетами
   *
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
