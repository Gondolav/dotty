object ListCharConcat {
  sealed trait List {
    dependent def ++(that: List): List =
      if (this.isInstanceOf[Nil.type]) that
      else Cons(this.asInstanceOf[Cons].head, this.asInstanceOf[Cons].tail ++ that)
  }
  dependent case object Nil extends List
  dependent case class Cons(head: Char, tail: List) extends List
}

object CheckParens {
    import ListCharConcat._

    dependent private def check(cs: List, opened: Int): Boolean =
        if (cs.isInstanceOf[Nil.type]) opened == 0
        else if (cs.asInstanceOf[Cons].head == '(') check(cs.asInstanceOf[Cons].tail, opened + 1)
        else if (cs.asInstanceOf[Cons].head == ')' && opened < 1) false
        else if (cs.asInstanceOf[Cons].head == ')') check(cs.asInstanceOf[Cons].tail, opened - 1)
        else check(cs.asInstanceOf[Cons].tail, opened)

    // Dependent function definition inside another function doesn't work, nor named parameters
    dependent def checkParens(s: List): Boolean = check(s, 0)

    val balancedEmpty: true = checkParens(Nil)
    val balanced1: true = checkParens(Cons('(', Cons('(', Cons(')', Cons(')', Nil)))))
    val balanced2: true = checkParens(Cons('a', Cons('(', Cons('s', Cons('(', Cons('v', Cons('v', Cons(')', Cons('d', Cons(')', Cons('f', Cons('f', Nil))))))))))))

    val notBalanced1: false = checkParens(Cons('(', Cons('(', Cons(')', Nil))))
    val notBalanced2: false = checkParens(Cons('(', Cons('(', Cons(')', Cons(')', Cons(')', Cons('(', Nil)))))))
}
