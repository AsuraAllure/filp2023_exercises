package exercises02

import scala.collection.mutable
object Counter {

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] = {
    val reg        = raw"[^\s.,!?:\n\t\r()]+".r
    val listOfWord = reg.findAllMatchIn(text).toList.map(matchObject => matchObject.toString().toLowerCase())
    val wordMap    = mutable.Map[String, Int]()
    for (word <- listOfWord) {
      if (wordMap.keySet.contains(word)) {
        wordMap(word) = wordMap(word) + 1
      } else {
        wordMap += (word -> 1)
      }
    }
    wordMap.toMap
  }

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] = {
    val reg        = raw"[^\s.,!?:\n\t\r()а-яА-Я]+".r
    val listOfWord = reg.findAllMatchIn(text).toList.map(matchObject => matchObject.toString().toLowerCase())
    val wordMap    = mutable.Map[String, Int]()
    for (word <- listOfWord) {
      if (wordMap.keySet.contains(word)) {
        wordMap(word) = wordMap(word) + 1
      } else {
        wordMap += (word -> 1)
      }
    }
    wordMap.toMap
  }

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] = {
    val reg        = raw"\d+[.,]?[\d+]?".r
    val listOfWord = reg.findAllMatchIn(text).toList.map(matchObject => matchObject.toString().toLowerCase())
    val wordMap    = mutable.Map[String, Int]()
    for (word <- listOfWord) {
      if (wordMap.keySet.contains(word)) {
        wordMap(word) = wordMap(word) + 1
      } else {
        wordMap += (word -> 1)
      }
    }
    wordMap.toMap
  }

}
