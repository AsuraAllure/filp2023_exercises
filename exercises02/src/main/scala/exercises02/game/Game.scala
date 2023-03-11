package exercises02.game

import scala.annotation.tailrec

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
    controller.askNumber()
    playing(number, controller.nextLine())
  }

  @tailrec
  private def playing(number: Int, input: String): Unit = {
    input.toIntOption match {
      case None =>
        if (input == GameController.IGiveUp) {
          controller.giveUp(number)
        } else {
          controller.wrongInput()
          controller.askNumber()
          playing(number, controller.nextLine())
        }
      case inpNumber =>
        if (inpNumber.get == number) {
          controller.guessed()
        } else {
          if (inpNumber.get < number) controller.numberIsBigger() else controller.numberIsSmaller()
          controller.askNumber()
          playing(number, controller.nextLine())
        }
    }
  }
}
