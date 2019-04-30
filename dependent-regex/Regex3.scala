object Lst {
    import Regex.StarMatch

    sealed trait Lst {
        /** Returns a new list resulting from the concatenation of this list with the given one.
         *
         *  @param that the list to concatenate.
         *  @return a list containing the elements from the left hand operand followed by the elements from the right hand operand.
         */
        def ++(that: Lst): Lst = this match {
            case Nil => that
            case Cons(x, xs) => Cons(x, xs ++ that)
        }

        /** Converts this list to a scala.collection.immutable.List.
         *
         *  @return a scala.collection.immutable.List containing all elements of this list.
         */
        def toList: List[Any] = this match {
            case Nil => scala.collection.immutable.Nil
            case Cons(x, xs) => x :: xs.toList
        }
    }

    case object Nil extends Lst
    case class Cons[T, Tail <: Lst](head: T, tail: Tail) extends Lst

    type Concat[L1 <: Lst, L2 <: Lst] <: Lst = L1 match {
            case Nil.type => L2
            case Cons[x, xs] => Cons[x, Concat[xs, L2]]
    }

    type ToTypesList[L <: Lst] <: Lst = L match {
        case Nil.type => Nil.type
        case Cons[x, xs] => x match {
            case 'C' => Cons[Char, ToTypesList[xs]]
            case 'S' => Cons[String, ToTypesList[xs]]
            case 'I' => Cons[Int, ToTypesList[xs]]
            case 'H' => Cons[Option[Char], ToTypesList[xs]]
            case 'T' => Cons[Option[String], ToTypesList[xs]]
            case 'N' => Cons[Option[Int], ToTypesList[xs]]
            case 'R' => Cons[Option[StarMatch[Char]], ToTypesList[xs]]
            case 'G' => Cons[Option[StarMatch[String]], ToTypesList[xs]]
            case _ => Cons[Option[StarMatch[Int]], ToTypesList[xs]]
        }
    }
}

object Nat {
    sealed trait Nat

    class Zero extends Nat

    class Suc[N <: Nat] extends Nat

    class Pred[N <: Nat] extends Nat
}

object CheckDelimiters {
    import Lst._
    import Nat._

    type CheckParens[Input <: Lst] = Check[Input, Zero, '(', ')']

    type CheckBrackets[Input <: Lst] = Check[Input, Zero, '[', ']']

    type Check[Input <: Lst, Opened <: Nat, Open <: Char, Close <: Char] = Input match {
        case Nil.type => Opened match {
            case Zero => true
            case _ => false
        }
        case Cons[Open, xs] => Opened match {
            case Pred[o] => Check[xs, o, Open, Close]
            case _ => Check[xs, Suc[Opened], Open, Close]
        }
        case Cons[Close, xs] => Opened match {
                case Zero => false
                case _ => Opened match {
                    case Suc[o] => Check[xs, o, Open, Close]
                    case _ => Check[xs, Pred[Opened], Open, Close]
                }
            }
        case Cons[_, xs] => Check[xs, Opened, Open, Close]
    }

    val balancedEmpty: CheckParens[Nil.type] = true
    val balanced1: CheckParens[Cons['(', Cons['(', Cons[')', Cons[')', Nil.type]]]]] = true
    val balanced2: CheckParens[Cons['a', Cons['(', Cons['s', Cons['(', Cons['v', Cons['v', Cons[')', Cons['d', Cons[')', Cons['f', Cons['f', Nil.type]]]]]]]]]]]] = true
    val balanced3: CheckParens[Cons['(', Cons[')', Cons['(', Cons[')', Nil.type]]]]] = true

    val notBalanced1: CheckParens[Cons['(', Cons['(', Cons[')', Nil.type]]]] = false
    val notBalanced2: CheckParens[Cons['(', Cons['(', Cons[')', Cons[')', Cons[')', Cons['(', Nil.type]]]]]]] = false

    val balancedBrackets1: CheckBrackets[Cons['[', Cons['[', Cons[']', Cons[']', Nil.type]]]]] = true
}

object Regex {
    import Lst._
    import CheckDelimiters._
    import Nat._

    sealed trait Type
    case object Str extends Type
    case object Integ extends Type
    case object Chr extends Type
    case object Empty extends Type
    case class Optional[T <: Type](tp: T) extends Type
    case class Star[T <: Type](tp: T) extends Type

    case object RegexError
    case class StarMatch[T](m: T)

    type CompileRegex[Input <: Lst] = CheckParens[Input] match {
        case true => Compile[Input, Empty.type, Zero, false, Zero, Nil.type, Input]
        case false => RegexError.type
    }

    type Compile[Input <: Lst, CurrType <: Type, Chars <: Nat, CharClass <: Boolean, Classes <: Nat, GroupsTypesRepr <: Lst, CachedRegex <: Lst] = CharClass match {
        case false => Input match {
            case Nil.type => ReturnType[GroupsTypesRepr]
            case Cons['[', xs] => Compile[xs, CurrType, Chars, true, Suc[Classes], GroupsTypesRepr, CachedRegex]
            case Cons['(', xs] => Compile[xs, Empty.type, Zero, false, Zero, GroupsTypesRepr, CachedRegex]
            case Cons[')', xs] => xs match {
                case Cons['?', xss] => Compile[xss, CurrType, Chars, CharClass, Classes, AddTypeToList[Optional[CurrType], GroupsTypesRepr, Chars], CachedRegex]
                case Cons['*', xss] => Compile[xss, CurrType, Chars, CharClass, Classes, AddTypeToList[Star[CurrType], GroupsTypesRepr, Chars], CachedRegex]
                case _ => Compile[xs, CurrType, Chars, CharClass, Classes, AddTypeToList[CurrType, GroupsTypesRepr, Chars], CachedRegex]
            }
            case Cons[x, xs] => IsDigit[x] match {
                case true => CurrType match {
                    case Empty.type => Compile[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]
                    case Integ.type => Compile[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]
                    case _ => Compile[xs, Str.type, Suc[Chars], CharClass, Classes, GroupsTypesRepr, CachedRegex]
                }
                case false => Compile[xs, Str.type, Suc[Chars], CharClass, Classes, GroupsTypesRepr, CachedRegex]
            }
        }
        case true => CheckBrackets[CachedRegex] match {
            case false => RegexError.type
            case true => CompileCharClass[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, ' ', CachedRegex]
        }
    }

    type CompileCharClass[Input <: Lst, CurrType <: Type, Chars <: Nat, CharClass <: Boolean, Classes <: Nat, GroupsTypesRepr <: Lst, FirstElemInClass, CachedRegex <: Lst] = Input match {
        case Cons['-', xs] => CompileCharClass[xs, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, FirstElemInClass, CachedRegex]
        case Cons[']', xs] => Classes match {
            case Suc[Zero] => CurrType match {
                case Str.type => Compile[xs, Chr.type, Chars, false, Classes, GroupsTypesRepr, CachedRegex]
                case _ => Compile[xs, CurrType, Chars, false, Classes, GroupsTypesRepr, CachedRegex]
            }
            case _ => Compile[xs, CurrType, Chars, false, Classes, GroupsTypesRepr, CachedRegex]
        }
        case Cons[x, xs] => IsDigit[x] match {
            case true => CurrType match {
                case Empty.type => CompileCharClass[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, x, CachedRegex]
                case Integ.type => CompileCharClass[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, x, CachedRegex]
                case _ => CompileCharClass[xs, Str.type, Chars, CharClass, Classes, GroupsTypesRepr, x, CachedRegex]
            }
            case false => CompileCharClass[xs, Str.type, Chars, CharClass, Classes, GroupsTypesRepr, x, CachedRegex]
        }
    }

    type AddTypeToList[T <: Type, L <: Lst, Chars <: Nat] <: Lst = T match {
        case Optional[t] => AddType[t, L, Chars, Cons['H', Nil.type], Cons['T', Nil.type], Cons['N', Nil.type]]
        case Star[t] => AddType[t, L, Chars, Cons['R', Nil.type], Cons['G', Nil.type], Cons['E', Nil.type]]
        case _ => AddType[T, L, Chars, Cons['C', Nil.type], Cons['S', Nil.type], Cons['I', Nil.type]]
    }

    type AddType[T <: Type, L <: Lst, Chars <: Nat, ReprC <: Lst, ReprS <: Lst, ReprI <: Lst] <: Lst = T match {
        case Chr.type => Concat[L, ReprC]
        case Str.type => Chars match {
                case Suc[Zero] => Concat[L, ReprC]
                case _ => Concat[L, ReprS]
            }
        case _ => Concat[L, ReprI]
    }

    type IsDigit[C] <: Boolean = C match {
        case '0' => true
        case '1' => true
        case '2' => true
        case '3' => true
        case '4' => true
        case '5' => true
        case '6' => true
        case '7' => true
        case '8' => true
        case '9' => true
        case _ => false
    }

    type ReturnType[GroupsTypesRepr <: Lst] = String => Option[ToTypesList[GroupsTypesRepr]]

    /** Returns a compiled regular expression pattern for the given regex.
     *
     *  @param regex the regular expression to compile into a pattern.
     *  @return a compiled regular expression pattern.
     */
    def compileRegex[Input <: Lst](regex: Input): CompileRegex[Input] = {
        val regexL = regex.toList
        compile(regexL, Empty, 0, false, 0, Nil, regexL)
    }.asInstanceOf[CompileRegex[Input]]

    private def compile[Input <: Lst, CurrType <: Type, Chars <: Nat, CharClass <: Boolean, Classes <: Nat, GroupsTypesRepr <: Lst, CachedRegex <: Lst](regex: List[Any], currType: CurrType, chars: Int, charClass: CharClass, classes: Int, groupsTypesRepr: GroupsTypesRepr, cachedRegex: => List[Any]): Compile[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex] = {
        if (charClass) compileCharClass(regex, currType, chars, charClass, classes, groupsTypesRepr, ' ', cachedRegex)
        else regex match {
            case scala.collection.immutable.Nil => returnType[GroupsTypesRepr](cachedRegex, groupsTypesRepr.toList)
            case '[' :: xs => compile(xs, currType, chars, true, classes + 1, groupsTypesRepr, cachedRegex)
            case '(' :: xs => compile(xs, Empty, 0, false, 0, groupsTypesRepr, cachedRegex)
            case ')' :: xs => xs match {
                case '?' :: xss => compile(xss, currType, chars, charClass, classes, addTypeToList(Optional(currType), groupsTypesRepr, chars), cachedRegex)
                case '*' :: xss => compile(xss, currType, chars, charClass, classes, addTypeToList(Star(currType), groupsTypesRepr, chars), cachedRegex)
                case _ => compile(xs, currType, chars, charClass, classes, addTypeToList(currType, groupsTypesRepr, chars), cachedRegex)
            }
            case x :: xs =>
                if (x.asInstanceOf[Char].isDigit && (currType.isInstanceOf[Empty.type] || currType.isInstanceOf[Integ.type])) compile(xs, Integ, chars, charClass, classes, groupsTypesRepr, cachedRegex)
                else compile(xs, Str, chars + 1, charClass, classes, groupsTypesRepr, cachedRegex)
        }
    }.asInstanceOf[Compile[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]]

    private def compileCharClass[Input <: Lst, CurrType <: Type, Chars <: Nat, CharClass <: Boolean, Classes <: Nat, GroupsTypesRepr <: Lst, FirstElemInClass <: Char, CachedRegex <: Lst](regex: List[Any], currType: CurrType, chars: Int, charClass: CharClass, classes: Int, groupsTypesRepr: GroupsTypesRepr, firstElemInClass: Char, cachedRegex: => List[Any]): CompileCharClass[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, FirstElemInClass, CachedRegex] = {
        (regex: @unchecked) match {
            case '-' :: xs => compileCharClass(xs, currType, chars, charClass, classes, groupsTypesRepr, firstElemInClass, cachedRegex)
            case ']' :: xs =>
                if (classes == 1 && currType.isInstanceOf[Str.type]) compile(xs, Chr, chars, false, classes, groupsTypesRepr, cachedRegex)
                else compile(xs, currType, chars, false, classes, groupsTypesRepr, cachedRegex)
            case x :: xs =>
                if (firstElemInClass > x.asInstanceOf[Char]) RegexError
                else if (x.asInstanceOf[Char].isDigit && (currType.isInstanceOf[Empty.type] || currType.isInstanceOf[Integ.type])) compileCharClass(xs, Integ, chars, charClass, classes, groupsTypesRepr, x.asInstanceOf[Char], cachedRegex)
                else compileCharClass(xs, Str, chars, charClass, classes, groupsTypesRepr, x.asInstanceOf[Char], cachedRegex)
        }
    }.asInstanceOf[CompileCharClass[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, FirstElemInClass, CachedRegex]]

    // Adds the given type to the given list by concatenating to it its char representation
    private def addTypeToList(t: Type, l: Lst, chars: Int): Lst = {
        def addType(t: Type, l: Lst, chars: Int, reprC: Lst, reprS: Lst, reprI: Lst) = t match {
            case Chr => l ++ reprC
            case Str =>
                if (chars == 1) l ++ reprC
                else l ++ reprS
            case _ => l ++ reprI
        }

        t match {
            case Optional(tp) => addType(tp, l, chars, Cons('H', Nil), Cons('T', Nil), Cons('N', Nil))
            case Star(tp) => addType(tp, l, chars, Cons('R', Nil), Cons('G', Nil), Cons('E', Nil))
            case _ => addType(t, l, chars, Cons('C', Nil), Cons('S', Nil), Cons('I', Nil))
        }
    }

    // Converts the given sequence to a Lst
    private def toLst(s: Seq[Any]): Lst =
        if (s.isEmpty) Nil
        else Cons(s.head, toLst(s.tail))

    // Builds and returns the closure that can be used as a pattern to match a given string
    private def returnType[ReturnTypesRepr <: Lst](regex: List[Any], returnTypesRepr: List[Any]): ReturnType[ReturnTypesRepr] =
        {
            input: String =>
                val firstMatchOpt = regex.mkString.r.findFirstMatchIn(input)
                if (firstMatchOpt.isEmpty) None
                else {
                    val firstMatch = firstMatchOpt.get
                    Some(toLst(returnTypesRepr.zipWithIndex.map {
                            case ('S', i) => firstMatch.group(i + 1).toString // String
                            case ('I', i) => firstMatch.group(i + 1).toInt // Int
                            case ('C', i) => firstMatch.group(i + 1)(0) // Char
                            case ('T', i) =>
                                val group = firstMatch.group(i + 1)
                                if (group == null) None
                                else Some(group.toString) // Option[String]
                            case ('G', i) =>
                                val group = firstMatch.group(i + 1)
                                if (group == null) None
                                else Some(StarMatch[String](group.toString)) // StarMatch[String]
                            case ('N', i) =>
                                val group = firstMatch.group(i + 1)
                                if (group == null) None
                                else Some(group.toInt) // Option[Int]
                            case ('E', i) =>
                                val group = firstMatch.group(i + 1)
                                if (group == null) None
                                else Some(StarMatch[Int](group.toInt)) // StarMatch[Int]
                            case ('H', i) =>
                                val group = firstMatch.group(i + 1)
                                if (group == null) None
                                else Some(group(0)) // Option[Char]
                            case ('R', i) =>
                                val group = firstMatch.group(i + 1)
                                if (group == null) None
                                else Some(StarMatch[Char](group(0))) // StarMatch[Char]
                        }))
                    }
        }.asInstanceOf[ReturnType[ReturnTypesRepr]]


    val myPattern1: String => Option[Cons[String, Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons['s', Cons['d', Cons['f', Cons['s', Cons[')', Cons['(', Cons['a', Cons[')', Nil.type]]]]]]]]]]](Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons(')', Nil)))))))))))
    val r1: String = (myPattern1("asdfsa"): @unchecked) match {
        case None => "none"
        case Some(Cons(s, Cons(c, Nil))) => s.asInstanceOf[String]
    }

    val myPattern2: String => Option[Cons[Int, Nil.type]] = compileRegex[Cons['(', Cons['1', Cons['2', Cons['3', Cons[')', Nil.type]]]]]](Cons('(', Cons('1', Cons('2', Cons('3', Cons(')', Nil))))))
    val r2: Int = (myPattern2("123"): @unchecked) match {
        case None => -1
        case Some(Cons(i, Nil)) => i
    }

    val myPattern3: String => Option[Cons[Char, Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons[')', Nil.type]]]]]]]](Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons(')', Nil))))))))
    val r3: Char = (myPattern3("f"): @unchecked) match {
        case None => 'n'
        case Some(Cons(c, Nil)) => c.asInstanceOf[Char]
    }

    val myPattern4: String => Option[Cons[String, Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons[')', Nil.type]]]]]]]]]]]]](Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Nil)))))))))))))
    val r4: String = (myPattern4("s0"): @unchecked) match {
        case None => "none"
        case Some(Cons(s, Nil)) => s.asInstanceOf[String]
    }

    val myPattern5: String => Option[Cons[String, Nil.type]] = compileRegex[Cons['(', Cons['a', Cons['[', Cons['a', Cons['-', Cons['b', Cons[']', Cons['0', Cons['[', Cons['8', Cons['-', Cons['9', Cons[']', Cons[')', Nil.type]]]]]]]]]]]]]]](Cons('(', Cons('a', Cons('[', Cons('a', Cons('-', Cons('b', Cons(']', Cons('0', Cons('[', Cons('8', Cons('-', Cons('9', Cons(']', Cons(')', Nil)))))))))))))))
    val r5: String = (myPattern5("ab09"): @unchecked) match {
        case None => "none"
        case Some(Cons(s, Nil)) => s.asInstanceOf[String]
    }

    val myPattern6: String => Option[Cons[Option[String], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons['b', Cons[')', Cons['?', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('a', Cons('b', Cons(')', Cons('?', Cons('(', Cons('c', Cons(')', Nil)))))))))
    val r6: (String, Char) = (myPattern6("abc"): @unchecked) match {
        case None => ("none", 'n')
        case Some(Cons(s, Cons(c, Nil))) => {
            if (s.asInstanceOf[Option[String]].isEmpty) ("none", c.asInstanceOf[Char])
            else (s.asInstanceOf[Option[String]].get, c.asInstanceOf[Char])
        }
    }

    val myPattern7: String => Option[Cons[Option[StarMatch[String]], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons['b', Cons[')', Cons['*', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('a', Cons('b', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil)))))))))
    val r7: (StarMatch[String], Char) = (myPattern7("ababc"): @unchecked) match {
        case None => (StarMatch("None"), 'n')
        case Some(Cons(s, Cons(c, Nil))) => {
            if (s.asInstanceOf[Option[StarMatch[String]]].isEmpty) (StarMatch("None"), c.asInstanceOf[Char])
            else (s.asInstanceOf[Option[StarMatch[String]]].get, c.asInstanceOf[Char])
        }
    }

    val myPattern8: String => Option[Cons[Option[StarMatch[Int]], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['1', Cons['2', Cons[')', Cons['*', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('1', Cons('2', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil)))))))))
    val r8: (StarMatch[Int], Char) = (myPattern8("12121212c"): @unchecked) match {
        case None => (StarMatch(0), 'n')
        case Some(Cons(s, Cons(c, Nil))) => {
            if (s.asInstanceOf[Option[StarMatch[Int]]].isEmpty) (StarMatch(0), c.asInstanceOf[Char])
            else (s.asInstanceOf[Option[StarMatch[Int]]].get, c.asInstanceOf[Char])
        }
    }

    val myPattern9: String => Option[Cons[Int, Cons[Option[StarMatch[Char]], Nil.type]]] = compileRegex[Cons['(', Cons['1', Cons['2', Cons[')', Cons['(', Cons['c', Cons[')', Cons['*', Nil.type]]]]]]]]](Cons('(', Cons('1', Cons('2', Cons(')', Cons('(', Cons('c', Cons(')', Cons('*', Nil)))))))))
    val r9: (Int, StarMatch[Char]) = (myPattern9("12ccc"): @unchecked) match {
        case None => (0, StarMatch('n'))
        case Some(Cons(s, Cons(c, Nil))) => {
            if (c.asInstanceOf[Option[StarMatch[Char]]].isEmpty) (0, StarMatch('n'))
            else (s.asInstanceOf[Int], c.asInstanceOf[Option[StarMatch[Char]]].get)
        }
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
    }
}

object RegexTests {
    import Lst._
    import Regex._

    val x0: RegexError.type = compileRegex[Cons['(', Nil.type]](Cons('(', Nil))
    val x1: String => Option[Cons[String, Nil.type]] = compileRegex[Cons['(', Cons['a', Cons['s', Cons['d', Cons['f', Cons['s', Cons[')', Nil.type]]]]]]]](Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Nil))))))))
    val x2: String => Option[Cons[Char, Nil.type]] = compileRegex[Cons['(', Cons['a', Cons[')', Nil.type]]]](Cons('(', Cons('a', Cons(')', Nil)))) 
    val x3: String => Option[Cons[Int, Nil.type]] = compileRegex[Cons['(', Cons['1', Cons['2', Cons['3', Cons[')', Nil.type]]]]]](Cons('(', Cons('1', Cons('2', Cons('3', Cons(')', Nil))))))
    val x4: String => Option[Cons[String, Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons[')', Nil.type]]]]]]]]]]]]](Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons(')', Nil)))))))))))))
    val x5: String => Option[Cons[Int, Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons[')', Nil.type]]]]]]]]]]]]](Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Nil)))))))))))))
    val x6: String => Option[Cons[Int, Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons[')', Nil.type]]]]]]]](Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Nil))))))))
    val x7: String => Option[Cons[Char, Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['A', Cons['-', Cons['Z', Cons[']', Cons[')', Nil.type]]]]]]]](Cons('(', Cons('[', Cons('A', Cons('-', Cons('Z', Cons(']', Cons(')', Nil))))))))
    val x8: String => Option[Cons[String, Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons['s', Cons['d', Cons['f', Cons['s', Cons[')', Cons['(', Cons['a', Cons[')', Nil.type]]]]]]]]]]](Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons(')', Nil)))))))))))
    val x9: String => Option[Cons[Int, Cons[String, Nil.type]]] = compileRegex[Cons['(', Cons['1', Cons['2', Cons['3', Cons['4', Cons['5', Cons[')', Cons['(', Cons['a', Cons['b', Cons[')', Nil.type]]]]]]]]]]]](Cons('(', Cons('1', Cons('2', Cons('3', Cons('4', Cons('5', Cons(')', Cons('(', Cons('a', Cons('b', Cons(')', Nil))))))))))))
    val x10: String => Option[Cons[Int, Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['[', Cons['1', Cons['-', Cons['3', Cons[']', Cons[')', Cons['(', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons[')', Nil.type]]]]]]]]]]]]]]](Cons('(', Cons('[', Cons('1', Cons('-', Cons('3', Cons(']', Cons(')', Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons(')', Nil)))))))))))))))
    val x11: String => Option[Cons[Int, Cons[String, Nil.type]]] = compileRegex[Cons['(', Cons['[', Cons['1', Cons['-', Cons['3', Cons[']', Cons[')', Cons['(', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['[', Cons['a', Cons['-', Cons['b', Cons[']', Cons[')', Nil.type]]]]]]]]]]]]]]]]]]]](Cons('(', Cons('[', Cons('1', Cons('-', Cons('3', Cons(']', Cons(')', Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('a', Cons('-', Cons('b', Cons(']', Cons(')', Nil))))))))))))))))))))
    val x12: String => Option[Cons[String, Cons[String, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons['s', Cons['d', Cons['f', Cons['s', Cons[')', Cons['(', Cons['a', Cons['b', Cons[')', Nil.type]]]]]]]]]]]](Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons('b', Cons(')', Nil))))))))))))
    val x13: String => Option[Cons[String, Cons[Char, Cons[Char, Nil.type]]]] = compileRegex[Cons['(', Cons['a', Cons['s', Cons['d', Cons['f', Cons['s', Cons[')', Cons['(', Cons['a', Cons[')', Cons['(', Cons['e', Cons[')', Nil.type]]]]]]]]]]]]]](Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons(')', Cons('(', Cons('e', Cons(')', Nil))))))))))))))
    val x14: String => Option[Cons[Option[String], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons['b', Cons[')', Cons['?', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('a', Cons('b', Cons(')', Cons('?', Cons('(', Cons('c', Cons(')', Nil)))))))))
    val x15: String => Option[Cons[Option[Char], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons[')', Cons['?', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]](Cons('(', Cons('a', Cons(')', Cons('?', Cons('(', Cons('c', Cons(')', Nil))))))))
    val x16: String => Option[Cons[Option[Int], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['1', Cons[')', Cons['?', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]](Cons('(', Cons('1', Cons(')', Cons('?', Cons('(', Cons('c', Cons(')', Nil))))))))
    val x17: String => Option[Cons[Option[Int], Cons[Option[Char], Nil.type]]] = compileRegex[Cons['(', Cons['1', Cons[')', Cons['?', Cons['(', Cons['c', Cons[')', Cons['?', Nil.type]]]]]]]]](Cons('(', Cons('1', Cons(')', Cons('?', Cons('(', Cons('c', Cons(')', Cons('?', Nil)))))))))
    val x18: String => Option[Cons[Option[StarMatch[String]], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons['b', Cons[')', Cons['*', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('a', Cons('b', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil)))))))))
    val x19: String => Option[Cons[Option[StarMatch[Char]], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons[')', Cons['*', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]](Cons('(', Cons('a', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil))))))))
    val x20: String => Option[Cons[Option[StarMatch[Int]], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['1', Cons[')', Cons['*', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]](Cons('(', Cons('1', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil))))))))
    val x21: String => Option[Cons[Option[StarMatch[Int]], Cons[Option[StarMatch[Char]], Nil.type]]] = compileRegex[Cons['(', Cons['1', Cons[')', Cons['*', Cons['(', Cons['c', Cons[')', Cons['*', Nil.type]]]]]]]]](Cons('(', Cons('1', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Cons('*', Nil)))))))))
}
