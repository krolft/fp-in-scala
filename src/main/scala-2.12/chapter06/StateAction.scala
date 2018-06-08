package chapter06


case class StateAction[S, +A](run: S => (A, S)) {

  import StateAction.unit

  def map[B](f: A => B): StateAction[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](st: StateAction[S, B], f: (A, B) => C): StateAction[S, C] =
    flatMap(a => st.map(b => f(a, b)))

  def flatMap[B](f: A => StateAction[S, B]): StateAction[S, B] = StateAction(state => {
    val (a, nextState) = run(state)
    f(a).run(nextState)
  })
}

object StateAction {
  def unit[S, A](a: A): StateAction[S, A] = StateAction(s => (a, s))

  def sequence[S, A](actions: List[StateAction[S, A]]): StateAction[S, List[A]] = StateAction(startState => {
    actions.foldRight(List[A]() -> startState) {
      case (action, (list, state)) =>
        val (a, nextState) = action.run(state)
        (a :: list, nextState)
    }
  })

  def modify[S](f: S => S): StateAction[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: StateAction[S, S] = StateAction(s => (s, s))

  def set[S](s: S): StateAction[S, Unit] = StateAction(_ => ((), s))
}
