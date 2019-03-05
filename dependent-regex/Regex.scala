object ListCharConcat {
  sealed trait List {
    dependent def ++(that: List): List =
      if (this.isInstanceOf[Nil.type]) that
      else Cons(this.asInstanceOf[Cons].head, this.asInstanceOf[Cons].tail ++ that)

    dependent def length: Int =
        if (this.isInstanceOf[Nil.type]) 0
        else 1 + this.asInstanceOf[Cons].tail.length
  }

  dependent case object Nil extends List
  dependent case class Cons(head: Char, tail: List) extends List

  val x : {1} = Cons('a', Nil).length
}

object CheckParens {
    import ListCharConcat._

    dependent private def check(cs: List, opened: Int, open: Char, close: Char): Boolean =
        if (cs.isInstanceOf[Nil.type]) opened == 0
        else if (cs.asInstanceOf[Cons].head == open) check(cs.asInstanceOf[Cons].tail, opened + 1, open, close)
        else if (cs.asInstanceOf[Cons].head == close && opened < 1) false
        else if (cs.asInstanceOf[Cons].head == close) check(cs.asInstanceOf[Cons].tail, opened - 1, open, close)
        else check(cs.asInstanceOf[Cons].tail, opened, open, close)

    dependent def checkParens(s: List): Boolean = check(s, 0, '(', ')')

    dependent def checkBrackets(s: List): Boolean = check(s, 0, '[', ']')

    val balancedEmpty: true = checkParens(Nil)
    val balanced1: true = checkParens(Cons('(', Cons('(', Cons(')', Cons(')', Nil)))))
    val balanced2: true = checkParens(Cons('a', Cons('(', Cons('s', Cons('(', Cons('v', Cons('v', Cons(')', Cons('d', Cons(')', Cons('f', Cons('f', Nil))))))))))))
    val balanced3: true = checkParens(Cons('(', Cons(')', Cons('(', Cons(')', Nil)))))

    val notBalanced1: false = checkParens(Cons('(', Cons('(', Cons(')', Nil))))
    val notBalanced2: false = checkParens(Cons('(', Cons('(', Cons(')', Cons(')', Cons(')', Cons('(', Nil)))))))

    val balancedBrackets1: true = checkParens(Cons('[', Cons('[', Cons(']', Cons(']', Nil)))))
}

object Regex {
    import ListCharConcat._
    import CheckParens.{checkParens, checkBrackets}

    sealed trait Type
    dependent case object Str extends Type {
        override def toString(): String = "String"
    }
    dependent case object Integ extends Type {
        override def toString(): String = "Int"
    }
    dependent case object Chr extends Type {
        override def toString(): String = "Char"
    }

    dependent def compileRegex(s: List): Any = compile(s, Str)

    dependent private def compile(s: List, currType: Type): Any = {
        if (!checkParens(s)) compile(s, currType)

        if (s.isInstanceOf[Nil.type]) returnType(currType)
        else if (s.asInstanceOf[Cons].head == '(' || s.asInstanceOf[Cons].head == ')') compile(s.asInstanceOf[Cons].tail, currType)
        else if (!isDigit(s.asInstanceOf[Cons].head) && s.asInstanceOf[Cons].tail.asInstanceOf[Cons].head == ')') null.asInstanceOf[String => Char]
        else if (isDigit(s.asInstanceOf[Cons].head)) compile(s.asInstanceOf[Cons].tail, Integ)
        else compile(s.asInstanceOf[Cons].tail, Str)
    }

    dependent private def isDigit(c: Char): Boolean = '0' <= c && c <= '9'

    dependent private def returnType(t: Type) =
        if (t.isInstanceOf[Str.type]) null.asInstanceOf[String => String]
        else null.asInstanceOf[String => Int]

    val x1: String => String = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Nil))))))))
    val x2: String => Char = compileRegex(Cons('(', Cons('a', Cons(')', Nil))))
    val x3: String => Int = compileRegex(Cons('(', Cons('1', Cons('2', Cons('3', Nil)))))

    // val x4: String => String = compileRegex("[a-z][a-z]")
    // val x5: String => Int = compileRegex("[0-9]")
    // val x6: String => Char = compileRegex("[A-Z]")
}
