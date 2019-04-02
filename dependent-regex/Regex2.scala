object ListCharConcat {
    sealed trait List {
        dependent def ++(that: List): List =
            if (this.isInstanceOf[Nil.type]) that
            else Cons(this.asInstanceOf[Cons].head, this.asInstanceOf[Cons].tail ++ that)

        dependent def init: List =
            if (this.isInstanceOf[Nil.type]) throw new UnsupportedOperationException("init of empty list")
            else if (this.asInstanceOf[Cons].tail.isInstanceOf[Nil.type]) Nil
            else Cons(this.asInstanceOf[Cons].head, this.asInstanceOf[Cons].tail.init)

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
            else if (c == 'I') 0
            else if (c == 'H') Some('c')
            else if (c == 'T') Some("s")
            else Some(0)
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
    dependent case object Empty extends Type {
        override def toString(): String = "Empty"
    }

    dependent case class Optional(tp: Type) extends Type

    dependent case object RegexError

    dependent def compileRegex(s: List): Any = {
        if (checkParens(s)) compile(s, Empty, 0, false, 0, Nil, s)
        else RegexError
    }

    dependent private def compile(s: List, currType: Type, chars: Int, charClass: Boolean, classes: Int, groupsTypesRepr: List, cachedRegex: => List): Any = {
        if (s.isInstanceOf[Nil.type]) returnType(cachedRegex, groupsTypesRepr)
        else if (charClass) {
            if (checkBrackets(cachedRegex)) compileCharClass(s, currType, chars, charClass, classes, groupsTypesRepr, cachedRegex, ' ')
            else RegexError
        } else {
            if (s.asInstanceOf[Cons].head == '[') compile(s.asInstanceOf[Cons].tail, currType, chars, true, classes + 1, groupsTypesRepr, cachedRegex)
            else if (s.asInstanceOf[Cons].head == '(') compile(s.asInstanceOf[Cons].tail, Empty, 0, false, 0, groupsTypesRepr, cachedRegex)
            else if (s.asInstanceOf[Cons].head == ')') compile(s.asInstanceOf[Cons].tail, currType, chars, charClass, classes, addTypeToList(currType, groupsTypesRepr, chars), cachedRegex)
            else if (s.asInstanceOf[Cons].head == '?') compile(s.asInstanceOf[Cons].tail, currType, chars, charClass, classes, addTypeToList(Optional(currType), groupsTypesRepr.asInstanceOf[Cons].init, chars), cachedRegex)
            else if (isDigit(s.asInstanceOf[Cons].head) && (currType.isInstanceOf[Empty.type] || currType.isInstanceOf[Integ.type])) compile(s.asInstanceOf[Cons].tail, Integ, chars, charClass, classes, groupsTypesRepr, cachedRegex)
            else compile(s.asInstanceOf[Cons].tail, Str, chars + 1, charClass, classes, groupsTypesRepr, cachedRegex)
        }
    }

    dependent private def compileCharClass(s: List, currType: Type, chars: Int, charClass: Boolean, classes: Int, groupsTypesRepr: List, cachedRegex: => List, firstElemInClass: Char): Any = {
        if (s.asInstanceOf[Cons].head == '-') compileCharClass(s.asInstanceOf[Cons].tail, currType, chars, charClass, classes, groupsTypesRepr, cachedRegex, firstElemInClass)
        else if (s.asInstanceOf[Cons].head == ']') {
            if (classes == 1 && currType.isInstanceOf[Str.type]) compile(s.asInstanceOf[Cons].tail, Chr, chars, false, classes, groupsTypesRepr, cachedRegex)
            else compile(s.asInstanceOf[Cons].tail, currType, chars, false, classes, groupsTypesRepr, cachedRegex)
        }
        else if (firstElemInClass > s.asInstanceOf[Cons].head) RegexError
        else if (isDigit(s.asInstanceOf[Cons].head) && (currType.isInstanceOf[Empty.type] || currType.isInstanceOf[Integ.type])) compileCharClass(s.asInstanceOf[Cons].tail, Integ, chars, charClass, classes, groupsTypesRepr, cachedRegex, s.asInstanceOf[Cons].head)
        else compileCharClass(s.asInstanceOf[Cons].tail, Str, chars, charClass, classes, groupsTypesRepr, cachedRegex, s.asInstanceOf[Cons].head)
    }

    dependent private def isDigit(c: Char): Boolean = '0' <= c && c <= '9'

    dependent private def addTypeToList(t: Type, l: List, chars: Int): List = {
        if (t.isInstanceOf[Optional]) {
            val tp = t.asInstanceOf[Optional].tp
            if (chars == 1 || tp.isInstanceOf[Chr.type]) l ++ Cons('H', Nil)
            else if (tp.isInstanceOf[Str.type]) l ++ Cons('T', Nil)
            else l ++ Cons('N', Nil)
        }
        else if (chars == 1 || t.isInstanceOf[Chr.type]) l ++ Cons('C', Nil)
        else if (t.isInstanceOf[Str.type]) l ++ Cons('S', Nil)
        else l ++ Cons('I', Nil)
    }

    def toListA(s: Seq[Any]): ListA = {
        if (s.isEmpty) NilA
        else ConsA(s.head, toListA(s.tail))
    }

    dependent private def returnType(regex: List, returnTypesRepr: List): String => Option[{ returnTypesRepr.toTypesList }] =
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
                            case (s) if s._1 == 'T' =>
                                val group = firstMatch.group(s._2 + 1)
                                if (group == null) None
                                else Some(firstMatch.group(s._2 + 1).toString)
                            case (s) if s._1 == 'N' =>
                                val group = firstMatch.group(s._2 + 1)
                                if (group == null) None
                                else Some(firstMatch.group(s._2 + 1).toInt)
                            case (s) if s._1 == 'H' =>
                                val group = firstMatch.group(s._2 + 1)
                                if (group == null) None
                                else Some(firstMatch.group(s._2 + 1)(0))
                        }))
                    }
        }.asInstanceOf[String => Option[{ returnTypesRepr.toTypesList }]]

    val myPattern1: String => Option[{ ConsA(??? : String, ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons(')', Nil)))))))))))
    val r1: String = (myPattern1("asdfsa"): @unchecked) match {
        case None => "none"
        case Some(ConsA(s, ConsA(c, NilA))) => s.asInstanceOf[String]
    }

    val myPattern2: String => Option[{ ConsA(??? : Int, NilA) }] = compileRegex(Cons('(', Cons('1', Cons('2', Cons('3', Cons(')', Nil))))))
    val r2: Int = (myPattern2("123"): @unchecked) match {
        case None => -1
        case Some(ConsA(i: Int, NilA)) => i
    }

    val myPattern3: String => Option[{ ConsA(??? : Char, NilA) }] = compileRegex(Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons(')', Nil))))))))
    val r3: Char = (myPattern3("f"): @unchecked) match {
        case None => 'n'
        case Some(ConsA(c, NilA)) => c.asInstanceOf[Char]
    }

    val myPattern4: String => Option[{ ConsA(??? : String, NilA) }] = compileRegex(Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Nil)))))))))))))
    val r4: String = (myPattern4("s0"): @unchecked) match {
        case None => "none"
        case Some(ConsA(s, NilA)) => s.asInstanceOf[String]
    }

    val myPattern5: String => Option[{ ConsA(??? : String, NilA) }] = compileRegex(Cons('(', Cons('a', Cons('[', Cons('a', Cons('-', Cons('b', Cons(']', Cons('0', Cons('[', Cons('8', Cons('-', Cons('9', Cons(']', Cons(')', Nil)))))))))))))))
    val r5: String = (myPattern5("ab09"): @unchecked) match {
        case None => "none"
        case Some(ConsA(s, NilA)) => s.asInstanceOf[String]
    }

    val myPattern6: String => Option[{ ConsA(??? : Option[String], ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('a', Cons('b', Cons(')', Cons('?', Cons('(', Cons('c', Cons(')', Nil)))))))))
    val r6: (String, Char) = (myPattern6("abc"): @unchecked) match {
        case None => ("none", 'n')
        case Some(ConsA(s, ConsA(c, NilA))) => {
            if (s.asInstanceOf[Option[String]].isEmpty) ("none", c.asInstanceOf[Char])
            else (s.asInstanceOf[Option[String]].get, c.asInstanceOf[Char])
        }
    }

    def main(args: Array[String]): Unit = {
        assert(r1 == "asdfs", s"Found $r1, expected asdfs")
        assert(r2 == 123, s"Found $r2, expected 123")
        assert(r3 == 'f', s"Found $r3, expected f")
        assert(r4 == "s0", s"Found $r4, expected s0")
        assert(r5 == "ab09", s"Found $r5, expected ab09")
        assert(r6 == ("ab", 'c'), s"Found $r6, expected (ab, c)")
    }
}

object RegexTests {
    import Regex.compileRegex
    import ListCharConcat._

    val x1: String => Option[{ ConsA(??? : String, NilA) }] = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Nil))))))))

    val x2: String => Option[{ ConsA(??? : Char, NilA) }] = compileRegex(Cons('(', Cons('a', Cons(')', Nil))))

    val x3: String => Option[{ ConsA(??? : Int, NilA) }] = compileRegex(Cons('(', Cons('1', Cons('2', Cons('3', Cons(')', Nil))))))

    val x4: String => Option[{ ConsA(??? : String, NilA) }] = compileRegex(Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons(')', Nil)))))))))))))

    val x5: String => Option[{ ConsA(??? : Int, NilA) }] = compileRegex(Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Nil)))))))))))))

    val x6: String => Option[{ ConsA(??? : Int, NilA) }] = compileRegex(Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Nil))))))))

    val x7: String => Option[{ ConsA(??? : Char, NilA) }] = compileRegex(Cons('(', Cons('[', Cons('A', Cons('-', Cons('Z', Cons(']', Cons(')', Nil))))))))

    val x8: String => Option[{ ConsA(??? : String, ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons(')', Nil)))))))))))

    val x9: String => Option[{ ConsA(??? : Int, ConsA(??? : String, NilA)) }] = compileRegex(Cons('(', Cons('1', Cons('2', Cons('3', Cons('4', Cons('5', Cons(')', Cons('(', Cons('a', Cons('b', Cons(')', Nil))))))))))))

    val x10: String => Option[{ ConsA(??? : Int, ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('[', Cons('1', Cons('-', Cons('3', Cons(']', Cons(')', Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons(')', Nil)))))))))))))))

    val x11: String => Option[{ ConsA(??? : Int, ConsA(??? : String, NilA)) }] = compileRegex(Cons('(', Cons('[', Cons('1', Cons('-', Cons('3', Cons(']', Cons(')', Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('a', Cons('-', Cons('b', Cons(']', Cons(')', Nil))))))))))))))))))))

    val x12: String => Option[{ ConsA(??? : String, ConsA(??? : String, NilA)) }] = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons('b', Cons(')', Nil))))))))))))

    val x13: String => Option[{ ConsA(??? : String, ConsA(??? : Char, ConsA(??? : Char, NilA))) }] = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons(')', Cons('(', Cons('e', Cons(')', Nil))))))))))))))

    val x14: String => Option[{ ConsA(??? : Option[String], ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('a', Cons('b', Cons(')', Cons('?', Cons('(', Cons('c', Cons(')', Nil)))))))))

    val x15: String => Option[{ ConsA(??? : Option[Char], ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('a', Cons(')', Cons('?', Cons('(', Cons('c', Cons(')', Nil))))))))

    val x16: String => Option[{ ConsA(??? : Option[Int], ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('1', Cons(')', Cons('?', Cons('(', Cons('c', Cons(')', Nil))))))))

    val x17: String => Option[{ ConsA(??? : Option[Int], ConsA(??? : Option[Char], NilA)) }] = compileRegex(Cons('(', Cons('1', Cons(')', Cons('?', Cons('(', Cons('c', Cons(')', Cons('?', Nil)))))))))
}
