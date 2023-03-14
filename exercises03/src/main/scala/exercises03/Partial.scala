package exercises03

object Partial {
  def combo[I, T](funcs: List[PartialFunction[I, T]]): I => Option[T] =
    (arg: I) =>
      funcs.foldLeft(PartialFunction.empty[I, T])((arg1, arg2) => if (arg1.lift(arg).isEmpty) arg2 else arg1).lift(arg)

}
