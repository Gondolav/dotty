object Lst {

    // Dependently-typed list representing a list of Char.
    sealed trait LstChar {
        /** Returns a new list resulting from the concatenation of this list with the given one.
         *
         *  @param that the list to concatenate.
         *  @return a list containing the elements from the left hand operand followed by the elements from the right hand operand.
         */
        dependent def ++(that: LstChar): LstChar =
            if (this.isInstanceOf[Nil.type]) that
            else Cons(this.asInstanceOf[Cons].head, this.asInstanceOf[Cons].tail ++ that)

        /** Converts this list to a scala.collection.immutable.List.
         *
         *  @return a scala.collection.immutable.List containing all elements of this list.
         */
        def toList: List[Char] =
            if (this.isInstanceOf[Nil.type]) scala.collection.immutable.Nil
            else this.asInstanceOf[Cons].head :: this.asInstanceOf[Cons].tail.toList

        override def toString: String = {
            val builder = StringBuilder.newBuilder

            def toStringAux(list: LstChar): Unit = {
                if (!list.isInstanceOf[Nil.type]) {
                    builder.append(list.asInstanceOf[Cons].head)
                    toStringAux(list.asInstanceOf[Cons].tail)
                }
            }

            toStringAux(this)
            builder.toString
        }

        /** Converts this list to a LstA.
         *
         *  @return a LstA containing all elements of this list.
         */
        dependent def toLstA: LstA = {
            if (this.isInstanceOf[Nil.type]) NilA
            else ConsA(toType(this.asInstanceOf[Cons].head), this.asInstanceOf[Cons].tail.toLstA)
        }

        /** Converts the given char to an Option wrapping the type it represents.
         *
         *  Possible char-type mappings are:
         *  - 'C' -> Char
         *  - 'S' -> String
         *  - 'I' -> Int
         *  - 'H' -> Option[Char]
         *  - 'T' -> Option[String]
         *  - 'N' -> Option[Int]
         *  - 'R' -> StarMatch[Char]
         *  - 'G' -> StarMatch[String]
         *  - 'E' -> StarMatch[Int]
         *
         *  @param c the char representation of a type.
         *  @return the corresponding type wrapped in Some().
         */
        dependent def toType(c: Char) = {
            import Regex.StarMatch

            if (c == 'C') c
            else if (c == 'S') "s"
            else if (c == 'I') 0
            else if (c == 'H') Some('c')
            else if (c == 'T') Some("s")
            else if (c == 'N') Some(0)
            else if (c == 'R') Some(StarMatch[Char]('c'))
            else if (c == 'G') Some(StarMatch[String]("s"))
            else Some(StarMatch[Int](0))
        }
    }

    dependent case object Nil extends LstChar
    dependent case class Cons(head: Char, tail: LstChar) extends LstChar

    // Dependently-typed list representing a list of Any.
    sealed trait LstA
    dependent case object NilA extends LstA
    dependent case class ConsA(head: Any, tail: LstA) extends LstA
}

object CheckDelimiters {
    import Lst._

    dependent private def check(cs: LstChar, opened: Int, open: Char, close: Char): Boolean = {
        if (cs.isInstanceOf[Nil.type]) opened == 0
        else if (cs.asInstanceOf[Cons].head == open) check(cs.asInstanceOf[Cons].tail, opened + 1, open, close)
        else if (cs.asInstanceOf[Cons].head == close && opened < 1) false
        else if (cs.asInstanceOf[Cons].head == close) check(cs.asInstanceOf[Cons].tail, opened - 1, open, close)
        else check(cs.asInstanceOf[Cons].tail, opened, open, close)
    }

    /** Returns true if the parentheses contained in the given string are balanced, false otherwise.
     *
     *  @param s the string containing the parentheses.
     *  @return true if the parentheses are balanced, false otherwise.
     */
    dependent def checkParens(s: LstChar): Boolean = check(s, 0, '(', ')')

    /** Returns true if the brackets contained in the given string are balanced, false otherwise.
     *
     *  @param s the string containing the brackets.
     *  @return true if the brackets are balanced, false otherwise.
     */
    dependent def checkBrackets(s: LstChar): Boolean = check(s, 0, '[', ']')

    val balancedEmpty: true = checkParens(Nil)
    val balanced1: true = checkParens(Cons('(', Cons('(', Cons(')', Cons(')', Nil)))))
    val balanced2: true = checkParens(Cons('a', Cons('(', Cons('s', Cons('(', Cons('v', Cons('v', Cons(')', Cons('d', Cons(')', Cons('f', Cons('f', Nil))))))))))))
    val balanced3: true = checkParens(Cons('(', Cons(')', Cons('(', Cons(')', Nil)))))

    val notBalanced1: false = checkParens(Cons('(', Cons('(', Cons(')', Nil))))
    val notBalanced2: false = checkParens(Cons('(', Cons('(', Cons(')', Cons(')', Cons(')', Cons('(', Nil)))))))

    val balancedBrackets1: true = checkBrackets(Cons('[', Cons('[', Cons(']', Cons(']', Nil)))))
}

object Regex {
    import Lst._
    import CheckDelimiters.{checkParens, checkBrackets}

    sealed trait Type
    dependent case object Str extends Type
    dependent case object Integ extends Type
    dependent case object Chr extends Type
    dependent case object Empty extends Type
    dependent case class Optional(tp: Type) extends Type
    dependent case class Star(tp: Type) extends Type

    dependent case object RegexError
    case class StarMatch[T](m: T)

    /** Returns a compiled regular expression pattern for the given regex.
     *
     *  The regex received as input should have each group enclosed in parentheses.
     *
     *  @param regex the regular expression to compile into a pattern.
     *  @return a compiled regular expression pattern.
     */
    dependent def compileRegex(regex: LstChar): Any = {
        if (checkParens(regex)) compile(regex, Empty, 0, false, 0, Nil, regex)
        else RegexError
    }

    dependent private def compile(regex: LstChar, currType: Type, chars: Int, charClass: Boolean, classes: Int, groupsTypesRepr: LstChar, cachedRegex: => LstChar): Any = {
        if (regex.isInstanceOf[Nil.type]) buildPattern(cachedRegex, groupsTypesRepr)
        else if (charClass) {
            if (checkBrackets(cachedRegex)) compileCharClass(regex, currType, chars, charClass, classes, groupsTypesRepr, cachedRegex, ' ', currType)
            else RegexError
        } else {
            if (regex.asInstanceOf[Cons].head == '[') compile(regex.asInstanceOf[Cons].tail, currType, chars, true, classes + 1, groupsTypesRepr, cachedRegex)
            else if (regex.asInstanceOf[Cons].head == '(') compile(regex.asInstanceOf[Cons].tail, Empty, 0, false, 0, groupsTypesRepr, cachedRegex)
            else if (regex.asInstanceOf[Cons].head == ')') {
                dependent val tailR = regex.asInstanceOf[Cons].tail
                if (tailR.isInstanceOf[Nil.type]) {
                    compile(tailR, currType, chars, charClass, classes, addTypeToList(currType, groupsTypesRepr, chars), cachedRegex)
                } else {
                    if (tailR.asInstanceOf[Cons].head == '*') compile(tailR.asInstanceOf[Cons].tail, currType, chars, charClass, classes, addTypeToList(Star(currType), groupsTypesRepr, chars), cachedRegex)
                    else if (tailR.asInstanceOf[Cons].head == '?') compile(tailR.asInstanceOf[Cons].tail, currType, chars, charClass, classes, addTypeToList(Optional(currType), groupsTypesRepr, chars), cachedRegex)
                    else compile(tailR, currType, chars, charClass, classes, addTypeToList(currType, groupsTypesRepr, chars), cachedRegex)
                }
            }
            else if (isDigit(regex.asInstanceOf[Cons].head) && (currType.isInstanceOf[Empty.type] || currType.isInstanceOf[Integ.type] || currType.isInstanceOf[Optional] || currType.isInstanceOf[Star])) {
                if (currType.isInstanceOf[Optional]) {
                    dependent val tp = currType.asInstanceOf[Optional].tp
                    if (tp.isInstanceOf[Integ.type]) compile(regex.asInstanceOf[Cons].tail, Integ, chars, charClass, classes, groupsTypesRepr, cachedRegex)
                    else compile(regex.asInstanceOf[Cons].tail, Str, chars + 1, charClass, classes, groupsTypesRepr, cachedRegex)
                }
                else if (currType.isInstanceOf[Star]) {
                    dependent val tp = currType.asInstanceOf[Star].tp
                    if (tp.isInstanceOf[Integ.type]) compile(regex.asInstanceOf[Cons].tail, Integ, chars, charClass, classes, groupsTypesRepr, cachedRegex)
                    else compile(regex.asInstanceOf[Cons].tail, Str, chars + 1, charClass, classes, groupsTypesRepr, cachedRegex)
                }
                else compile(regex.asInstanceOf[Cons].tail, Integ, chars, charClass, classes, groupsTypesRepr, cachedRegex)
            }
            else compile(regex.asInstanceOf[Cons].tail, Str, chars + 1, charClass, classes, groupsTypesRepr, cachedRegex)
        }
    }

    dependent private def compileCharClass(regex: LstChar, currType: Type, chars: Int, charClass: Boolean, classes: Int, groupsTypesRepr: LstChar, cachedRegex: => LstChar, firstElemInClass: Char, typeBeforeClass: Type): Any = {
        if (regex.asInstanceOf[Cons].head == '-') compileCharClass(regex.asInstanceOf[Cons].tail, currType, chars, charClass, classes, groupsTypesRepr, cachedRegex, firstElemInClass, typeBeforeClass)
        else if (regex.asInstanceOf[Cons].head == ']') {
            dependent val ifChar = if (classes == 1 && currType.isInstanceOf[Str.type]) Chr else currType
            dependent val tailR = regex.asInstanceOf[Cons].tail
            if (tailR.asInstanceOf[Cons].head == '*') compile(tailR.asInstanceOf[Cons].tail, if (typeBeforeClass.isInstanceOf[Empty.type]) Star(currType) else currType, chars, false, classes, groupsTypesRepr, cachedRegex)
            else if (tailR.asInstanceOf[Cons].head == '?') compile(tailR.asInstanceOf[Cons].tail, if (typeBeforeClass.isInstanceOf[Empty.type]) Optional(ifChar) else ifChar, chars, false, classes, groupsTypesRepr, cachedRegex)
            else compile(regex.asInstanceOf[Cons].tail, ifChar, chars, false, classes, groupsTypesRepr, cachedRegex)
        }
        else if (firstElemInClass > regex.asInstanceOf[Cons].head) RegexError
        else if (isDigit(regex.asInstanceOf[Cons].head) && (currType.isInstanceOf[Empty.type] || currType.isInstanceOf[Integ.type] || currType.isInstanceOf[Optional] || currType.isInstanceOf[Star])) {
                if (currType.isInstanceOf[Optional]) {
                    dependent val tp = currType.asInstanceOf[Optional].tp
                    if (tp.isInstanceOf[Integ.type]) compileCharClass(regex.asInstanceOf[Cons].tail, Integ, chars, charClass, classes, groupsTypesRepr, cachedRegex, regex.asInstanceOf[Cons].head, typeBeforeClass)
                    else compileCharClass(regex.asInstanceOf[Cons].tail, Str, chars, charClass, classes, groupsTypesRepr, cachedRegex, regex.asInstanceOf[Cons].head, typeBeforeClass)
                }
                else if (currType.isInstanceOf[Star]) {
                    dependent val tp = currType.asInstanceOf[Star].tp
                    if (tp.isInstanceOf[Integ.type]) compileCharClass(regex.asInstanceOf[Cons].tail, Integ, chars, charClass, classes, groupsTypesRepr, cachedRegex, regex.asInstanceOf[Cons].head, typeBeforeClass)
                    else compileCharClass(regex.asInstanceOf[Cons].tail, Str, chars, charClass, classes, groupsTypesRepr, cachedRegex, regex.asInstanceOf[Cons].head, typeBeforeClass)
                }
                else compileCharClass(regex.asInstanceOf[Cons].tail, Integ, chars, charClass, classes, groupsTypesRepr, cachedRegex, regex.asInstanceOf[Cons].head, typeBeforeClass)
            }
        else compileCharClass(regex.asInstanceOf[Cons].tail, Str, chars, charClass, classes, groupsTypesRepr, cachedRegex, regex.asInstanceOf[Cons].head, typeBeforeClass)
    }

    dependent private def isDigit(c: Char): Boolean = '0' <= c && c <= '9'

    // Adds the given type to the given list by concatenating to it its char representation
    dependent private def addTypeToList(t: Type, l: LstChar, chars: Int): LstChar = {
        if (t.isInstanceOf[Optional]) addType(t.asInstanceOf[Optional].tp, l, chars, Cons('H', Nil), Cons('T', Nil), Cons('N', Nil))
        else if (t.isInstanceOf[Star]) addType(t.asInstanceOf[Star].tp, l, chars, Cons('R', Nil), Cons('G', Nil), Cons('E', Nil))
        else addType(t, l, chars, Cons('C', Nil), Cons('S', Nil), Cons('I', Nil))
    }

    dependent private def addType(t: Type, l: LstChar, chars: Int, reprC: LstChar, reprS: LstChar, reprI: LstChar): LstChar = {
        if (t.isInstanceOf[Chr.type]) l ++ reprC
        else if (t.isInstanceOf[Str.type]) {
            if (chars == 1) l ++ reprC
            else l ++ reprS
        }
        else l ++ reprI
    }

    // Converts the given sequence to a LstA
    private def toLstA(s: Seq[Any]): LstA =
        if (s.isEmpty) NilA
        else ConsA(s.head, toLstA(s.tail))

    // Builds and returns the closure that can be used as a pattern to match a given string
    dependent private def buildPattern(regex: LstChar, groupsTypesRepr: LstChar): String => Option[{ groupsTypesRepr.toLstA }] =
        {
            input: String =>
                val firstMatchOpt = regex.toString.r.findFirstMatchIn(input)
                if (firstMatchOpt.isEmpty) None
                else {
                    val firstMatch = firstMatchOpt.get
                    Some(toLstA(groupsTypesRepr.toList.zipWithIndex.map {
                            case (s) if s._1 == 'S' => firstMatch.group(s._2 + 1).toString // String
                            case (s) if s._1 == 'I' => firstMatch.group(s._2 + 1).toInt // Int
                            case (s) if s._1 == 'C' => firstMatch.group(s._2 + 1)(0) // Char
                            case (s) if s._1 == 'T' =>
                                val group = firstMatch.group(s._2 + 1)
                                if (group == null) None
                                else Some(group.toString) // Option[String]
                            case (s) if s._1 == 'G' =>
                                val group = firstMatch.group(s._2 + 1)
                                if (group == null) None
                                else Some(StarMatch[String](group.toString)) // StarMatch[String]
                            case (s) if s._1 == 'N' =>
                                val group = firstMatch.group(s._2 + 1)
                                if (group == null) None
                                else Some(group.toInt) // Option[Int]
                            case (s) if s._1 == 'E' =>
                                val group = firstMatch.group(s._2 + 1)
                                if (group == null) None
                                else Some(StarMatch[Int](group.toInt)) // StarMatch[Int]
                            case (s) if s._1 == 'H' =>
                                val group = firstMatch.group(s._2 + 1)
                                if (group == null) None
                                else Some(group(0)) // Option[Char]
                            case (s) if s._1 == 'R' =>
                                val group = firstMatch.group(s._2 + 1)
                                if (group == null) None
                                else Some(StarMatch[Char](group(0))) // StarMatch[Char]
                        }))
                    }
        }.asInstanceOf[String => Option[{ groupsTypesRepr.toLstA }]]

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

    val myPattern7: String => Option[{ ConsA(??? : Option[StarMatch[String]], ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('a', Cons('b', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil)))))))))
    val r7: (StarMatch[String], Char) = (myPattern7("ababc"): @unchecked) match {
        case None => (StarMatch("None"), 'n')
        case Some(ConsA(s, ConsA(c, NilA))) => {
            if (s.asInstanceOf[Option[StarMatch[String]]].isEmpty) (StarMatch("None"), c.asInstanceOf[Char])
            else (s.asInstanceOf[Option[StarMatch[String]]].get, c.asInstanceOf[Char])
        }
    }

    val myPattern8: String => Option[{ ConsA(??? : Option[StarMatch[Int]], ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('1', Cons('2', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil)))))))))
    val r8: (StarMatch[Int], Char) = (myPattern8("12121212c"): @unchecked) match {
        case None => (StarMatch(0), 'n')
        case Some(ConsA(s, ConsA(c, NilA))) => {
            if (s.asInstanceOf[Option[StarMatch[Int]]].isEmpty) (StarMatch(0), c.asInstanceOf[Char])
            else (s.asInstanceOf[Option[StarMatch[Int]]].get, c.asInstanceOf[Char])
        }
    }

    val myPattern9: String => Option[{ ConsA(??? : Int, ConsA(??? : Option[StarMatch[Char]], NilA)) }] = compileRegex(Cons('(', Cons('1', Cons('2', Cons(')', Cons('(', Cons('c', Cons(')', Cons('*', Nil)))))))))
    val r9: (Int, StarMatch[Char]) = (myPattern9("12ccc"): @unchecked) match {
        case None => (0, StarMatch('n'))
        case Some(ConsA(s, ConsA(c, NilA))) => {
            if (c.asInstanceOf[Option[StarMatch[Char]]].isEmpty) (0, StarMatch('n'))
            else (s.asInstanceOf[Int], c.asInstanceOf[Option[StarMatch[Char]]].get)
        }
    }

    val myPattern10: String => Option[{ ConsA(??? : Option[StarMatch[String]], NilA) }] = compileRegex(Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('*', Cons(')', Nil)))))))))
    val r10: StarMatch[String] = (myPattern10("abc"): @unchecked) match {
        case None => StarMatch("none")
        case Some(ConsA(s, NilA)) => {
            if (s.asInstanceOf[Option[StarMatch[String]]].isEmpty) StarMatch("empty")
            else s.asInstanceOf[Option[StarMatch[String]]].get
        }
    }

    val myPattern11: String => Option[{ ConsA(??? : Option[StarMatch[Int]], NilA) }] = compileRegex(Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('*', Cons(')', Nil)))))))))
    val r11: StarMatch[Int] = (myPattern11("123"): @unchecked) match {
        case None => StarMatch(0)
        case Some(ConsA(s, NilA)) => {
            if (s.asInstanceOf[Option[StarMatch[Int]]].isEmpty) StarMatch(0)
            else s.asInstanceOf[Option[StarMatch[Int]]].get
        }
    }

    val myPattern12: String => Option[{ ConsA(??? : Int, NilA) }] = compileRegex(Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('?', Cons('1', Cons(')', Nil))))))))))
    val r12: Int = (myPattern12("1"): @unchecked) match {
        case None => 0
        case Some(ConsA(s, NilA)) => s.asInstanceOf[Int]
    }

    def main(args: Array[String]): Unit = {
        assert(r1 == "asdfs", s"Found $r1, expected asdfs")
        assert(r2 == 123, s"Found $r2, expected 123")
        assert(r3 == 'f', s"Found $r3, expected f")
        assert(r4 == "s0", s"Found $r4, expected s0")
        assert(r5 == "ab09", s"Found $r5, expected ab09")
        assert(r6 == ("ab", 'c'), s"Found $r6, expected (ab, c)")
        assert(r7 == (StarMatch("ab"), 'c'), s"Found $r7, expected (StarMatch(ab), c)")
        assert(r8 == (StarMatch(12), 'c'), s"Found $r8, expected (StarMatch(12), c)")
        assert(r9 == (12, StarMatch('c')), s"Found $r9, expected (12, StarMatch(c))")
        assert(r10 == StarMatch("abc"), s"Found $r10, expected StarMatch(abc)")
        assert(r11 == StarMatch(123), s"Found $r11, expected StarMatch(123)")
        assert(r12 == 1, s"Found $r12, expected 1")
    }
}

object RegexTests {
    import Lst._
    import Regex._

    val y1: RegexError.type = compileRegex(Cons('(', Nil))
    val y2: RegexError.type = compileRegex(Cons('(', Cons('[', Cons(')', Nil))))
    val y3: RegexError.type = compileRegex(Cons('(', Cons('[', Cons('z', Cons('-', Cons('a', Cons(']', Cons(')', Nil))))))))

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
    val x18: String => Option[{ ConsA(??? : Option[StarMatch[String]], ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('a', Cons('b', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil)))))))))
    val x19: String => Option[{ ConsA(??? : Option[StarMatch[Char]], ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('a', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil))))))))
    val x20: String => Option[{ ConsA(??? : Option[StarMatch[Int]], ConsA(??? : Char, NilA)) }] = compileRegex(Cons('(', Cons('1', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil))))))))
    val x21: String => Option[{ ConsA(??? : Option[StarMatch[Int]], ConsA(??? : Option[StarMatch[Char]], NilA)) }] = compileRegex(Cons('(', Cons('1', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Cons('*', Nil)))))))))
    val x22: String => Option[{ ConsA(??? : Option[StarMatch[String]], NilA) }] = compileRegex(Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('*', Cons(')', Nil)))))))))
    val x23: String => Option[{ ConsA(??? : Option[Char], NilA) }] = compileRegex(Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('?', Cons(')', Nil)))))))))
    val x24: String => Option[{ ConsA(??? : Option[StarMatch[Int]], NilA) }] = compileRegex(Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('*', Cons(')', Nil)))))))))
    val x25: String => Option[{ ConsA(??? : Option[Int], NilA) }] = compileRegex(Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('?', Cons(')', Nil)))))))))
    val x26: String => Option[{ ConsA(??? : Int, NilA) }] = compileRegex(Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('?', Cons(')', Nil))))))))))))))
}

// object Benchmarks {
//     import Lst._
//     import CheckDelimiters._
//     import Regex._


//     // dependent def createLstChar(length: Int): LstChar = createLstCharAux(length, Nil)

//     // dependent private def createLstCharAux(length: Int, acc: LstChar): LstChar = {
//     //     if (length == 0) acc
//     //     else createLstCharAux(length - 2, acc ++ Cons('(', Cons(')', Nil)))
//     // }

//     dependent def createRegex(length: Int): LstChar = createRegexAux(length, Nil)

//     dependent private def createRegexAux(length: Int, acc: LstChar): LstChar = {
//         if (length == 0) acc
//         else createRegexAux(length - 4, acc ++ Cons('(', Cons('a', Cons('z', Cons(')', Nil)))))
//     }

//     val balanced1: true = checkParens(createRegex(192))
// }

