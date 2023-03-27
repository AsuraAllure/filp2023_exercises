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
      if (holdString.nonEmpty) {
        if ((holdString.last == 'F' && newChar == 'f') ||
            (holdString.last == 'f' && newChar == 'F') ||
            (holdString.last == 'g' && newChar == 'G') ||
            (holdString.last == 'G' && newChar == 'g') ||
            (holdString.last == 'h' && newChar == 'H') ||
            (holdString.last == 'H' && newChar == 'h') ||
            (holdString.last == 'e' && newChar == 'E') ||
            (holdString.last == 'E' && newChar == 'e')) holdString.substring(0, holdString.length - 1)
        else
          holdString.appended(newChar)
      } else
        holdString.appended(newChar)
    )
  }

}
