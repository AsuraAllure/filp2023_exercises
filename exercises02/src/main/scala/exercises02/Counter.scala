package exercises02
object Counter {
  private final val wordRegex        = raw"[^\s.,!?:\n\t\r()]+".r
  private final val englishWordRegex = raw"[^\s.,!?:\n\t\r()а-яА-Я]+".r
  private final val numberRegex      = raw"\d+[.,]?[\d+]?".r

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] = {
    val listOfWord = wordRegex.findAllMatchIn(text).toList.map(matchObj => matchObj.toString().toLowerCase())
    listOfWord.groupMapReduce((b: String) => b)(_ => 1)((a: Int, b: Int) => a + b)
  }

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] = {
    val listOfWord =
      englishWordRegex.findAllMatchIn(text).toList.map(matchObject => matchObject.toString().toLowerCase())
    listOfWord.groupMapReduce((b: String) => b)(_ => 1)((a: Int, b: Int) => a + b)
  }

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] = {
    val listOfWord = numberRegex.findAllMatchIn(text).toList.map(matchObject => matchObject.toString().toLowerCase())
    listOfWord.groupMapReduce((b: String) => b)(_ => 1)((a: Int, b: Int) => a + b)
  }
}
