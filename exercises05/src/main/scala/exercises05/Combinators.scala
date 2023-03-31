package exercises05

object Combinators {
  // Есть цепочка hefEgGeGFEgGgeHE
  // в данной цепочке есть различные типы частиц
  // f, e, h, g положительно заряженные частицы
  // F, E, H, G отрицательно заряженные частицы
  // если частицы одного типа с разной полярностью стоят вместе в цепочке, они реагируют и исчезают
  // проход слева направо
  //
  // hefEgGeGFEgGgeHE <- gG прореагировали
  // hefEeGFEgGgeHE <- Ee прореагировали
  // hefGFEgGgeHE <- gG
  // hefGFEgeHE <- итоговая цепочка, в которой 10 частиц
  //
  // Напишите функцию, используя комбинаторы стандартной библиотеки,
  // которая проведёт полную реакцию
  def react(ipt: String): String = {
    // Через fold
    ipt.foldLeft("")((holdString, newChar) =>
      holdString.lastOption match {
        case None => newChar.toString
        case Some(lastChar) =>
          if ((lastChar.isLower && lastChar.toUpper == newChar) || (lastChar.isUpper && lastChar.toLower == newChar))
            holdString.substring(0, holdString.length - 1)
          else
            holdString.appended(newChar)
      }
    )
  }
}
