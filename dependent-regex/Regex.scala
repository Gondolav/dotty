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
        }

object CheckParens {
    import ListCharConcat._

    dependent private def check(cs: List, opened: Int, open: Char, close: Char): Boolean = {
        if (cs.isInstanceOf[Nil.type]) opened == 0
        else if (cs.asInstanceOf[Cons].head == open) check(cs.asInstanceOf[Cons].tail, opened + 1, open, close)
        else if (cs.asInstanceOf[Cons].head == close && opened < 1) false
        else if (cs.asInstanceOf[Cons].head == close) check(cs.asInstanceOf[Cons].tail, opened - 1, open, close)
        else check(cs.asInstanceOf[Cons].tail, opened, open, close)
    }

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

    dependent def compileRegex(s: List): Any = compile(s, Str, 0, false, 0)

    dependent private def compile(s: List, currType: Type, chars: Int, charClass: Boolean, classes: Int): Any = {
        if (!checkParens(s)) compile(s, currType, chars, charClass, classes)

        if (s.isInstanceOf[Nil.type]) returnType(currType, chars)
        else if (charClass) {
            if (isDigit(s.asInstanceOf[Cons].head)) compile(s.asInstanceOf[Cons].tail, Integ, chars, charClass, classes)
            else if (s.asInstanceOf[Cons].head == '-') compile(s.asInstanceOf[Cons].tail, currType, chars, charClass, classes)
            else if (s.asInstanceOf[Cons].head == ']') {
                if (classes == 1 && currType.isInstanceOf[Str.type]) compile(s.asInstanceOf[Cons].tail, Chr, chars, false, classes)
                else compile(s.asInstanceOf[Cons].tail, currType, chars, false, classes)
            }
            else compile(s.asInstanceOf[Cons].tail, Str, chars, charClass, classes)
        } else {
            if (s.asInstanceOf[Cons].head == '[') {
                if (!checkBrackets(s)) compile(s, currType, chars, charClass, classes)
                compile(s.asInstanceOf[Cons].tail, currType, chars, true, classes + 1)
            }
            else if (s.asInstanceOf[Cons].head == '(' || s.asInstanceOf[Cons].head == ')') compile(s.asInstanceOf[Cons].tail, currType, chars, charClass, classes)
            else if (isDigit(s.asInstanceOf[Cons].head)) compile(s.asInstanceOf[Cons].tail, Integ, chars, charClass, classes)
            else compile(s.asInstanceOf[Cons].tail, Str, chars + 1, charClass, classes)
        }
    }

    dependent private def isDigit(c: Char): Boolean = '0' <= c && c <= '9'

    dependent private def returnType(t: Type, chars: Int) = {
        if (chars == 1) null.asInstanceOf[String => Char]
        else if (t.isInstanceOf[Str.type]) null.asInstanceOf[String => String]
        else if (t.isInstanceOf[Chr.type]) null.asInstanceOf[String => Char]
        else null.asInstanceOf[String => Int]
    }

    //val x = compileRegex(Cons('(', Nil))
    val x1: String => String = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Nil))))))))
    val x2: String => Char = compileRegex(Cons('(', Cons('a', Cons(')', Nil))))
    val x3: String => Int = compileRegex(Cons('(', Cons('1', Cons('2', Cons('3', Nil)))))
    val x4: String => String = compileRegex(Cons('(', Cons('a', Cons('c', Cons('3', Cons('2', Cons('s', Cons(')', Nil))))))))

    val x5: String => String = compileRegex(Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons(')', Nil)))))))))))))
    val x6: String => Int = compileRegex(Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Nil)))))))))))))
    val x7: String => Int = compileRegex(Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Nil))))))))
    val x8: String => Char = compileRegex(Cons('(', Cons('[', Cons('A', Cons('-', Cons('Z', Cons(']', Cons(')', Nil))))))))
}
