package huffman

import scala.annotation.tailrec

/**
* Задание 4: Huffman coding
*
*/
object Huffman {

  /**
  * Код Хафмана представляется в виде бинарного дерева.
  *
  * Кждый листовой узел  `Leaf` представляет символ кодируемого алфавита.
  * Вес `Leaf` задает частоту появления символа в тексте.
  *
  * Ветви дерева, узлы `Fork`, кодируют множество всех букв, представляемых листьями этой ветви.
  * Вес `Fork` равен сумме всех ветвей
  */
  abstract class CodeTree(val weight: Int)
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], override val weight: Int) extends CodeTree(weight)
  case class Leaf(char: Char, override val weight: Int) extends CodeTree(weight)



  // Часть 1: Основа

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(_, weight) => weight
    case Fork(left, right, _, _) => weight(left) + weight(right)
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(char, _) => List(char)
    case Fork(left, right, _, _) => chars(left) ::: chars(right)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Часть 2: Генерирование дерева Хафмана

  /**
  * В этой части мы будем работать со списком симвалов. Эта функция позволяет создать список символов
  * из заданой строки.
  */
  def string2Chars(str: String): List[Char] = str.toList

  /**
  * Эта функция подсчитывает количиство фхождений каждого уникального символов из списка `chars` в этот же список.  
   * Например вызов
  *
  *   times(List('a', 'b', 'a'))
  *
  * должен вернуть следующее (порядок пар в списке не важен):
  *
  *   List(('a', 2), ('b', 1))
  *
  * Тип `List[(Char, Int)]` определяет список пар, в котором каждая пара включает символ и целое число вхождений этого символа
  * Пары могут быть созданы с помощью скобок:
  *
  *   val pair: (Char, Int) = ('c', 1)
  *
  * Для доступа к элементам пары можно использовать методы `_1` и `_2`:
  *
  *   val theChar = pair._1
  *   val theInt  = pair._2
  *
  * или сопоставление с образцом:
  *
  *   pair match {
    *     case (theChar, theInt) =>
    *       println("character is: "+ theChar)
    *       println("integer is  : "+ theInt)
    *   }
    */
    def times(chars: List[Char]): List[(Char, Int)] =  ???

    /**
    * Возвращает список узлов `Leaf` для заданной таблицы частот `freqs`.
    *
    * Возвращаемый список должен быть отсортирован в возрастающем порядке (в голове списка
      * должна стоять пара с минимальным весом), где вес пары - это частота вхождения символа.
    */
    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = ???

    /**
    * Проверяет что `trees` включает только одно дерево.
    */
    def singleton(trees: List[CodeTree]): Boolean = ???

    /**
    * Параметр `trees` содержит список `CodeTree`, отсортированный в возрастающем порядке весов.
    *
    * Функция принимает первые два элемента `trees` и комбинирует их в один узел
    * `Fork`. Этот узел добавляется в `trees` на свое место согласно весу.
    *
    * Если в `trees` меньше двух элементов, то он возвращается не измененным.
    */
    def combine(trees: List[CodeTree]): List[CodeTree] = ???
    /**
    * Эта функция вызывается следующим образом:
    *
    *   until(singleton, combine)(trees)
    *
    * где `trees` имеет тип `List[CodeTree]`, `singleton` и `combine` реализованы
    * ранее.
    *
    * Вызов `until` из примера выше должен вызывать две функции до тех пор, пока в списке не останется один элемент.
    * затем должен вернуть этот список.
    */
    def until(finishCondition: List[CodeTree] => Boolean, combineFunction: List[CodeTree] => List[CodeTree] )(trees: List[CodeTree]): CodeTree = 
      ???

    /**
    * Эта функция создает оптимальное дерево Хафмана для списка символов `chars`.
    *
    * В аргументе `chars` простой текст. Эта функция определяет частоту символов в тексте
    * и создает дерево на основе этой информации.
    */
    def createCodeTree(chars: List[Char]): CodeTree = ???

    // Часть 3: Декодирование

    type Bit = Int

    /**
    * Эта функция декадирует послкдовательность бит `bits` используя дерево кодирования `tree` и возвращает
     * результат в виде списка символов.
    */
    def decode(tree: CodeTree, bits: List[Bit]): List[Char] = ???

    /**
    * Дерево для французкого языка.
    * На основе данных
    *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
    */
    val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

    /**
    * Что это за сообщение? Можете декадировать?
    * Используйте `frenchCode`, определенное вверху.
    */
    val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

    /**
    * Напишите функцию декадирования зашифрованной фразы
    */
    def decodedSecret: List[Char] = ???



    // Часть 4a: Кодирование с использованием дерева Хаффмана

    /**
    * Кодирует `text` используя `tree`
    * в последовательность бит.
    */
    def encode(tree: CodeTree)(text: List[Char]): List[Bit] = ???

    // Часть 4b: Кодирование с использованием таблицы

    type CodeTable = List[(Char, List[Bit])]

    /**
    * Функция возвращает последовательность бит для символов `char` в таблице `table`.
    */
    def codeBits(table: CodeTable)(char: Char): List[Bit] =
      if (table.isEmpty) List()
      else if (table.head._1 == char) table.head._2
      else codeBits(table.tail)(char)

    /**
    * Строит таблицу, включающую каждый символ в дереве.
    * Последовательность бит представляет символ.
    */
    def convert(tree: CodeTree): CodeTable = ???

    /**
    * Объеденяет две таблицы символов в одну. В зависимости от того, как
    * вы используете этот метод в `convert`, объявленый сверху, этот метод может выполнять трансформации
    * над вргументами.
    */
    def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

    /**
    * Кодирует `text` согласно `tree`.
    *
    * Для ускорение кодирования сначала трансформирует дерево в таблицу
    * и только потом выполняет кодирование.
    */
    def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = ???
  }
