package exercises03

object Partial {
  def combo[I, T](funcs: List[PartialFunction[I, T]]): I => Option[T] =
    (arg: I) =>
      funcs.foldLeft(PartialFunction.empty[I, T])((func1, func2) => if (func2.isDefinedAt(arg)) func2 else func1).lift(arg)

}
