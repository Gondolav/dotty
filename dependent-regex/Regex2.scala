object ListCharConcat {
    sealed trait List {
        dependent def ++(that: List): List =
            if (this.isInstanceOf[Nil.type]) that
            else Cons(this.asInstanceOf[Cons].head, this.asInstanceOf[Cons].tail ++ that)

        def toSeq: Seq[Char] = {
            def toSeqAux(list: List, acc: Seq[Char]): Seq[Char] = {
                if (list.isInstanceOf[Nil.type]) acc
                else toSeqAux(list.asInstanceOf[Cons].tail, acc :+ list.asInstanceOf[Cons].head)
            }

            toSeqAux(this, Seq.empty[Char])
        }

        override def toString: String = {
            val builder = StringBuilder.newBuilder

            def toStringAux(list: List): Unit = {
                if (!list.isInstanceOf[Nil.type]) {
                    builder.append(list.asInstanceOf[Cons].head)
                    toStringAux(list.asInstanceOf[Cons].tail)
                }
            }

            toStringAux(this)
            builder.toString
        }

        dependent def toTypesList: ListA = {
            if (this.isInstanceOf[Nil.type]) NilA
            else ConsA(toType(this.asInstanceOf[Cons].head), this.asInstanceOf[Cons].tail.toTypesList)
        }

        dependent def toType(c: Char) = {
            if (c == 'C') c
            else if (c == 'S') "s"
            else 0
        }
    }
    dependent case object Nil extends List
    dependent case class Cons(head: Char, tail: List) extends List

    sealed trait ListA {
        dependent def ++(that: ListA): ListA =
            if (this.isInstanceOf[NilA.type]) that
            else ConsA(this.asInstanceOf[ConsA].head, this.asInstanceOf[ConsA].tail ++ that)
    }
    dependent case object NilA extends ListA
    dependent case class ConsA(head: Any, tail: ListA) extends ListA
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
    dependent case object RegexError

    dependent def compileRegex(s: List): Any = {
        if (checkParens(s)) compile(s, Str, 0, false, 0, Nil, s)
        else RegexError
    }

    dependent private def compile(s: List, currType: Type, chars: Int, charClass: Boolean, classes: Int, groupsTypesRepr: List, cachedRegex: => List): Any = {
        if (s.isInstanceOf[Nil.type]) returnType2(cachedRegex, groupsTypesRepr)
        else if (charClass) {
            if (isDigit(s.asInstanceOf[Cons].head)) {
                if (currType.isInstanceOf[Integ.type]) compile(s.asInstanceOf[Cons].tail, Integ, chars, charClass, classes, groupsTypesRepr, cachedRegex)
                else RegexError // TODO
            }
            else if (s.asInstanceOf[Cons].head == '-') compile(s.asInstanceOf[Cons].tail, currType, chars, charClass, classes, groupsTypesRepr, cachedRegex)
            else if (s.asInstanceOf[Cons].head == ']') {
                if (classes == 1 && currType.isInstanceOf[Str.type]) compile(s.asInstanceOf[Cons].tail, Chr, chars, false, classes, groupsTypesRepr, cachedRegex)
                else compile(s.asInstanceOf[Cons].tail, currType, chars, false, classes, groupsTypesRepr, cachedRegex)
            }
            else compile(s.asInstanceOf[Cons].tail, Str, chars, charClass, classes, groupsTypesRepr, cachedRegex)
        } else {
            if (s.asInstanceOf[Cons].head == '[') {
                if (!checkBrackets(s)) compile(s, currType, chars, charClass, classes, groupsTypesRepr, cachedRegex)
                compile(s.asInstanceOf[Cons].tail, currType, chars, true, classes + 1, groupsTypesRepr, cachedRegex)
            }
            else if (s.asInstanceOf[Cons].head == '(') compile(s.asInstanceOf[Cons].tail, Str, 0, false, 0, groupsTypesRepr, cachedRegex)
            else if (s.asInstanceOf[Cons].head == ')') compile(s.asInstanceOf[Cons].tail, currType, chars, charClass, classes, addTypeToList(currType, groupsTypesRepr, chars), cachedRegex)
            else if (isDigit(s.asInstanceOf[Cons].head)) compile(s.asInstanceOf[Cons].tail, Integ, chars, charClass, classes, groupsTypesRepr, cachedRegex)
            else compile(s.asInstanceOf[Cons].tail, Str, chars + 1, charClass, classes, groupsTypesRepr, cachedRegex)
        }
    }

    dependent private def isDigit(c: Char): Boolean = '0' <= c && c <= '9'

    dependent private def addTypeToList(t: Type, l: List, chars: Int): List = {
        if (chars == 1 || t.isInstanceOf[Chr.type]) l ++ Cons('C', Nil)
        else if (t.isInstanceOf[Str.type]) l ++ Cons('S', Nil)
        else l ++ Cons('I', Nil)
    }

    def toListA(s: Seq[Any]): ListA = {
        if (s.isEmpty) NilA
        else ConsA(s.head, toListA(s.tail))
    }

    dependent private def returnType2(regex: List, returnTypesRepr: List): String => Option[{ returnTypesRepr.toTypesList }] =
        {
            input: String =>
                val firstMatchOpt = regex.toString.r.findFirstMatchIn(input)
                if (firstMatchOpt.isEmpty) None
                else {
                    val firstMatch = firstMatchOpt.get
                    Some(toListA(returnTypesRepr.toSeq.zipWithIndex.map {
                            case (s) if s._1 == 'S' => firstMatch.group(s._2 + 1).toString
                            case (s) if s._1 == 'I' => firstMatch.group(s._2 + 1).toInt
                            case (s) if s._1 == 'C' => firstMatch.group(s._2 + 1)(0)
                        }))
                    }
        }.asInstanceOf[String => Option[{ returnTypesRepr.toTypesList }]]

    //val x: String => Int = compileRegex(Cons('(', Cons('[', Nil)))
    //val x1: String => String = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Nil))))))))
    // val x2: String => Char = compileRegex(Cons('(', Cons('a', Cons(')', Nil))))
    // val x3: String => Int = compileRegex(Cons('(', Cons('1', Cons('2', Cons('3', Nil)))))
    //
    // val x4: String => String = compileRegex(Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons(')', Nil)))))))))))))
    // val x5: String => Int = compileRegex(Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Nil)))))))))))))
    // val x6: String => Int = compileRegex(Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Nil))))))))
    // val x7: String => Char = compileRegex(Cons('(', Cons('[', Cons('A', Cons('-', Cons('Z', Cons(']', Cons(')', Nil))))))))
    // val x8: { ConsA(??? : String, ConsA(??? : Char, NilA)) } = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons(')', Nil)))))))))))

    val myPattern: String => Option[{ ConsA(??? : String, ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons(')', Nil)))))))))))
    val r1: String = (myPattern.apply("asdfsa"): @unchecked) match {
        case None => "none"
        case Some(ConsA(s, ConsA(c, NilA))) => s.asInstanceOf[String]
    }

    val myPattern2: String => Option[{ ConsA(??? : Int, NilA) }] = compileRegex(Cons('(', Cons('1', Cons('2', Cons('3', Cons(')', Nil))))))
    val r2: Int = (myPattern2.apply("123"): @unchecked) match {
        case None => -1
        case Some(ConsA(i: Int, NilA)) => i
    }

    // val x9: {Cons('I', Cons('S', Nil))} = compileRegex(Cons('(', Cons('1', Cons('2', Cons('3', Cons('4', Cons('5', Cons(')', Cons('(', Cons('a', Cons('b', Cons(')', Nil))))))))))))
    // val x10: {Cons('I', Cons('C', Nil))} = compileRegex(Cons('(', Cons('[', Cons('1', Cons('-', Cons('3', Cons(']', Cons(')', Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons(')', Nil)))))))))))))))
    // val x11: {Cons('I', Cons('S', Nil))} = compileRegex(Cons('(', Cons('[', Cons('1', Cons('-', Cons('3', Cons(']', Cons(')', Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('a', Cons('-', Cons('b', Cons(']', Cons(')', Nil))))))))))))))))))))
    // val x12: {Cons('S', Cons('S', Nil))} = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons('b', Cons(')', Nil))))))))))))
    // val x13: {Cons('S', Cons('C', Cons('C', Nil)))} = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons(')', Cons('(', Cons('e', Cons(')', Nil))))))))))))))

    def main(args: Array[String]): Unit = {
        println(s"r1: $r1")
        println(s"r2: $r2")
    }
}
