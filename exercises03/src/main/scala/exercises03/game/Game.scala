package exercises03.game

object Game {
  def parseState(input: String, number: Int): State = input.toIntOption match {
    case None =>
      if (input == GameController.IGiveUp) {
        GiveUp
      } else {
        WrongInput
      }
    case inputNumber =>
      if (inputNumber.get == number) {
        Guessed
      } else {
        if (inputNumber.get < number)
          NumberIsBigger
        else
          NumberIsSmaller
      }
  }

  def action(state: State, number: Int): GameController => Unit =
    (controller: GameController) =>
      state match {
        case Guessed         => controller.guessed()
        case GiveUp          => controller.giveUp(number)
        case WrongInput      => controller.wrongInput()
        case NumberIsBigger  => controller.numberIsBigger()
        case NumberIsSmaller => controller.numberIsSmaller()
      }

  def completed(state: State): Boolean = state match {
    case GiveUp | Guessed => true
    case _                => false
  }
}
