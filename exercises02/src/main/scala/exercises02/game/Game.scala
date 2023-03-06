package exercises02.game

class Game(controller: GameController) {

  /**
    * Игра угадай число
    * Ввод и вывод необходимо осуществлять с помощью методов controller
    *
    * Игра должна вызывать controller.askNumber перед каждой попыткой игрока угадать число
    * И вызвать controller.nextLine для получения ввода игрока
    * Если игрок ввел число меньше загаданного, игра должна вызвать controller.numberIsBigger
    * Если игрок ввел число больше загаданного, игра должна вызвать controller.numberIsSmaller
    * Если игрок угадал число, игра должна закончиться и вызвать controller.guessed
    * Если игрок написал GameController.IGiveUp, игра должна закончиться и вызвать controller.giveUp(number)
    * Если игрок ввел неизвестную комбинацию символов, надо вызвать contoller.wrongInput и продолжить игру
    *
    * @param number загаданное число
    */
  def play(number: Int): Unit = {
    do {
      controller.askNumber()
    } while (
      controller.nextLine() match {
        case GameController.IGiveUp =>
          controller.giveUp(number)
          false
        case input =>
          if (!input.forall(_.isDigit)) {
            controller.wrongInput()
            true
          } else {
            input.toInt.compare(number) match {
              case 1 =>
                controller.numberIsSmaller()
                true
              case 0 =>
                controller.guessed()
                false
              case -1 =>
                controller.numberIsBigger()
                true
            }
          }
      }
    )
  }
}
