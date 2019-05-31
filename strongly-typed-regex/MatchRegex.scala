object Lst {
    import Regex._

    // A trait representing a generic list.
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
            case Optional[t] =>
                ToType[t, Cons[Option[Char], ToTypesList[xs]], Cons[Option[String], ToTypesList[xs]], Cons[Option[Int], ToTypesList[xs]]]
            case Star[t] =>
                ToType[t, Cons[Option[StarMatch[String]], ToTypesList[xs]], Cons[Option[StarMatch[String]], ToTypesList[xs]], Cons[Option[StarMatch[Int]], ToTypesList[xs]]]
            case _ =>
                ToType[x, Cons[Char, ToTypesList[xs]], Cons[String, ToTypesList[xs]], Cons[Int, ToTypesList[xs]]]
        }
    }

    type ToType[T, C <: Lst, S <: Lst, I <: Lst] <: Lst = T match {
        case Chr.type => C
        case Str.type => S
        case Integ.type => I
    }
}

// Natural numbers are needed for type-level computations.
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
            case Nil.type => BuildPattern[GroupsTypesRepr]
            case Cons['[', xs] => Compile[xs, CurrType, Chars, true, Suc[Classes], GroupsTypesRepr, CachedRegex]
            case Cons['(', xs] => Compile[xs, Empty.type, Zero, false, Zero, GroupsTypesRepr, CachedRegex]
            case Cons[')', xs] => xs match {
                case Cons['*', xss] => Compile[xss, CurrType, Chars, CharClass, Classes, AddTypeToList[Star[CurrType], GroupsTypesRepr, Chars], CachedRegex]
                case Cons['?', xss] => Compile[xss, CurrType, Chars, CharClass, Classes, AddTypeToList[Optional[CurrType], GroupsTypesRepr, Chars], CachedRegex]
                case _ => Compile[xs, CurrType, Chars, CharClass, Classes, AddTypeToList[CurrType, GroupsTypesRepr, Chars], CachedRegex]
            }
            case Cons[x, xs] => IsDigit[x] match {
                case true => CurrType match {
                    case Empty.type => Compile[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]
                    case Integ.type => Compile[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]
                    case Optional[Integ.type] => Compile[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]
                    case Star[Integ.type] => Compile[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]
                    case _ => Compile[xs, Str.type, Suc[Chars], CharClass, Classes, GroupsTypesRepr, CachedRegex]
                }
                case false => Compile[xs, Str.type, Suc[Chars], CharClass, Classes, GroupsTypesRepr, CachedRegex]
            }
        }
        case true => CheckBrackets[CachedRegex] match {
            case false => RegexError.type
            case true => CompileCharClass[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, CurrType]
        }
    }

    type IfEmpty[Scrutinee <: Type, T1 <: Type, T2 <: Type] <: Type = Scrutinee match {
        case Empty.type => T1
        case _ => T2
    }

    type CompileCharClass[Input <: Lst, CurrType <: Type, Chars <: Nat, CharClass <: Boolean, Classes <: Nat, GroupsTypesRepr <: Lst, CachedRegex <: Lst, TypeBeforeClass <: Type] = Input match {
        case Cons['-', xs] => CompileCharClass[xs, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, TypeBeforeClass]
        case Cons[']', xs] => Classes match {
            case Suc[Zero] => CurrType match {
                case Str.type => xs match {
                    case Cons['*', xss] => Compile[xss, IfEmpty[TypeBeforeClass, Star[CurrType], CurrType], Chars, false, Classes, GroupsTypesRepr, CachedRegex]
                    case Cons['?', xss] => Compile[xss, IfEmpty[TypeBeforeClass, Optional[Chr.type], Chr.type], Chars, false, Classes, GroupsTypesRepr, CachedRegex]
                    case _ => Compile[xs, Chr.type, Chars, false, Classes, GroupsTypesRepr, CachedRegex]
                }
                case _ => xs match {
                    case Cons['*', xss] => Compile[xss, IfEmpty[TypeBeforeClass, Star[CurrType], CurrType], Chars, false, Classes, GroupsTypesRepr, CachedRegex]
                    case Cons['?', xss] => Compile[xss, IfEmpty[TypeBeforeClass, Optional[CurrType], CurrType], Chars, false, Classes, GroupsTypesRepr, CachedRegex]
                    case _ => Compile[xs, CurrType, Chars, false, Classes, GroupsTypesRepr, CachedRegex]
                }
            }
            case _ => xs match {
                    case Cons['*', xss] => Compile[xss, IfEmpty[TypeBeforeClass, Star[CurrType], CurrType], Chars, false, Classes, GroupsTypesRepr, CachedRegex]
                    case Cons['?', xss] => Compile[xss, IfEmpty[TypeBeforeClass, Optional[CurrType], CurrType], Chars, false, Classes, GroupsTypesRepr, CachedRegex]
                    case _ => Compile[xs, CurrType, Chars, false, Classes, GroupsTypesRepr, CachedRegex]
                }
        }
        case Cons[x, xs] => IsDigit[x] match {
            case true => CurrType match {
                case Empty.type => CompileCharClass[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, TypeBeforeClass]
                case Integ.type => CompileCharClass[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, TypeBeforeClass]
                case Optional[Integ.type] => CompileCharClass[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, TypeBeforeClass]
                case Star[Integ.type] => CompileCharClass[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, TypeBeforeClass]
                case _ => CompileCharClass[xs, Str.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, TypeBeforeClass]
            }
            case false => CompileCharClass[xs, Str.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, TypeBeforeClass]
        }
    }

    type AddTypeToList[T <: Type, L <: Lst, Chars <: Nat] <: Lst = T match {
        case Optional[t] => AddType[t, L, Chars, Cons[Optional[Chr.type], Nil.type], Cons[Optional[Str.type], Nil.type], Cons[Optional[Integ.type], Nil.type]]
        case Star[t] => AddType[t, L, Chars, Cons[Star[Str.type], Nil.type], Cons[Star[Str.type], Nil.type], Cons[Star[Integ.type], Nil.type]]
        case _ => AddType[T, L, Chars, Cons[Chr.type, Nil.type], Cons[Str.type, Nil.type], Cons[Integ.type, Nil.type]]
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

    type BuildPattern[GroupsTypesRepr <: Lst] = String => Option[ToTypesList[GroupsTypesRepr]]

    /** Returns a compiled regular expression pattern for the given regex.
     *
     *  The regex received as input should have each group enclosed in parentheses.
     *
     *  @param regex the regular expression to compile into a pattern.
     *  @return a compiled regular expression pattern.
     */
    def compileRegex[Input <: Lst](regex: Input): CompileRegex[Input] = {
        val regexL = regex.toList
        compile(regexL, Empty, 0, false, 0, Nil, regexL)
    }.asInstanceOf[CompileRegex[Input]]

    private def compile[Input <: Lst, CurrType <: Type, Chars <: Nat, CharClass <: Boolean, Classes <: Nat, GroupsTypesRepr <: Lst, CachedRegex <: Lst](regex: List[Any], currType: CurrType, chars: Int, charClass: CharClass, classes: Int, groupsTypesRepr: GroupsTypesRepr, cachedRegex: => List[Any]): Compile[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex] = {
        if (charClass) compileCharClass(regex, currType, chars, charClass, classes, groupsTypesRepr, ' ', cachedRegex, currType)
        else regex match {
            case scala.collection.immutable.Nil => buildPattern[GroupsTypesRepr](cachedRegex, groupsTypesRepr.toList)
            case '[' :: xs => compile(xs, currType, chars, true, classes + 1, groupsTypesRepr, cachedRegex)
            case '(' :: xs => compile(xs, Empty, 0, false, 0, groupsTypesRepr, cachedRegex)
            case ')' :: xs => xs match {
                case '*' :: xss => compile(xss, currType, chars, charClass, classes, addTypeToList(Star(currType), groupsTypesRepr, chars), cachedRegex)
                case '?' :: xss => compile(xss, currType, chars, charClass, classes, addTypeToList(Optional(currType), groupsTypesRepr, chars), cachedRegex)
                case _ => compile(xs, currType, chars, charClass, classes, addTypeToList(currType, groupsTypesRepr, chars), cachedRegex)
            }
            case x :: xs =>
                if (x.asInstanceOf[Char].isDigit && (currType == Empty || currType == Integ || currType == Optional(Integ) || currType == Star(Integ))) compile(xs, Integ, chars, charClass, classes, groupsTypesRepr, cachedRegex)
                else compile(xs, Str, chars + 1, charClass, classes, groupsTypesRepr, cachedRegex)
        }
    }.asInstanceOf[Compile[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]]

    private def compileCharClass[Input <: Lst, CurrType <: Type, Chars <: Nat, CharClass <: Boolean, Classes <: Nat, GroupsTypesRepr <: Lst, CachedRegex <: Lst, TypeBeforeClass <: Type](regex: List[Any], currType: CurrType, chars: Int, charClass: CharClass, classes: Int, groupsTypesRepr: GroupsTypesRepr, firstElemInClass: Char, cachedRegex: => List[Any], typeBeforeClass: TypeBeforeClass): CompileCharClass[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, TypeBeforeClass] = {
        (regex: @unchecked) match {
            case '-' :: xs => compileCharClass(xs, currType, chars, charClass, classes, groupsTypesRepr, firstElemInClass, cachedRegex, typeBeforeClass)
            case ']' :: xs => {
                val ifChar = if (classes == 1 && currType == Str) Chr else currType
                xs match {
                    case '*' :: xss => compile(xss, if (typeBeforeClass == Empty) Star(currType) else currType, chars, false, classes, groupsTypesRepr, cachedRegex)
                    case '?' :: xss => compile(xss, if (typeBeforeClass == Empty) Optional(ifChar) else ifChar, chars, false, classes, groupsTypesRepr, cachedRegex)
                    case _ => compile(xs, ifChar, chars, false, classes, groupsTypesRepr, cachedRegex)
                }
            }
            case x :: xs =>
                if (firstElemInClass > x.asInstanceOf[Char]) RegexError
                else if (x.asInstanceOf[Char].isDigit && (currType == Empty || currType == Integ || currType == Optional(Integ) || currType == Star(Integ))) compileCharClass(xs, Integ, chars, charClass, classes, groupsTypesRepr, x.asInstanceOf[Char], cachedRegex, typeBeforeClass)
                else compileCharClass(xs, Str, chars, charClass, classes, groupsTypesRepr, x.asInstanceOf[Char], cachedRegex, typeBeforeClass)
        }
    }.asInstanceOf[CompileCharClass[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, TypeBeforeClass]]

    // Adds the given type to the given list by concatenating to it its representation
    private def addTypeToList(t: Type, l: Lst, chars: Int): Lst = {
        def addType(t: Type, l: Lst, chars: Int, reprC: Lst, reprS: Lst, reprI: Lst) = t match {
            case Chr => l ++ reprC
            case Str =>
                if (chars == 1) l ++ reprC
                else l ++ reprS
            case _ => l ++ reprI
        }

        t match {
            case Optional(tp) => addType(tp, l, chars, Cons(Optional(Chr), Nil), Cons(Optional(Str), Nil), Cons(Optional(Integ), Nil))
            case Star(tp) => addType(tp, l, chars, Cons(Star(Str), Nil), Cons(Star(Str), Nil), Cons(Star(Integ), Nil))
            case _ => addType(t, l, chars, Cons(Chr, Nil), Cons(Str, Nil), Cons(Integ, Nil))
        }
    }

    // Converts the given sequence to a Lst
    private def toLst(s: Seq[Any]): Lst =
        if (s.isEmpty) Nil
        else Cons(s.head, toLst(s.tail))

    // Builds and returns the closure that can be used as a pattern to match a given string
    private def buildPattern[GroupsTypesRepr <: Lst](regex: List[Any], groupsTypesRepr: List[Any]): BuildPattern[GroupsTypesRepr] =
        {
            input: String =>
                val firstMatchOpt = regex.mkString.r.findFirstMatchIn(input)
                if (firstMatchOpt.isEmpty) None
                else {
                    val firstMatch = firstMatchOpt.get
                    Some(toLst(groupsTypesRepr.zipWithIndex.map {
                            case (Str, i) => firstMatch.group(i + 1).toString // String
                            case (Integ, i) => firstMatch.group(i + 1).toInt // Int
                            case (Chr, i) => firstMatch.group(i + 1)(0) // Char
                            case (Optional(Str), i) => Option(firstMatch.group(i + 1).toString) // Option[String]
                            case (Star(Str), i) => Option(StarMatch[String](firstMatch.group(i + 1).toString)) // StarMatch[String]
                            case (Optional(Integ), i) => Option(firstMatch.group(i + 1).toInt) // Option[Int]
                            case (Star(Integ), i) => Option(StarMatch[Int](firstMatch.group(i + 1).toInt)) // StarMatch[Int]
                            case (Optional(Chr), i) => Option(firstMatch.group(i + 1)(0)) // Option[Char]
                        }))
                    }
        }.asInstanceOf[BuildPattern[GroupsTypesRepr]]


    val myPattern1: String => Option[Cons[String, Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons['s', Cons['d', Cons['f', Cons['s', Cons[')', Cons['(', Cons['a', Cons[')', Nil.type]]]]]]]]]]](Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons(')', Nil)))))))))))
    val r1: String = (myPattern1("asdfsa"): @unchecked) match {
        case None => "none"
        case Some(Cons(s, Cons(c, Nil))) => s
    }

    val myPattern2: String => Option[Cons[Int, Nil.type]] = compileRegex[Cons['(', Cons['1', Cons['2', Cons['3', Cons[')', Nil.type]]]]]](Cons('(', Cons('1', Cons('2', Cons('3', Cons(')', Nil))))))
    val r2: Int = (myPattern2("123"): @unchecked) match {
        case None => -1
        case Some(Cons(i, Nil)) => i
    }

    val myPattern3: String => Option[Cons[Char, Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons[')', Nil.type]]]]]]]](Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons(')', Nil))))))))
    val r3: Char = (myPattern3("f"): @unchecked) match {
        case None => 'n'
        case Some(Cons(c, Nil)) => c
    }

    val myPattern4: String => Option[Cons[String, Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons[')', Nil.type]]]]]]]]]]]]](Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Nil)))))))))))))
    val r4: String = (myPattern4("s0"): @unchecked) match {
        case None => "none"
        case Some(Cons(s, Nil)) => s
    }

    val myPattern5: String => Option[Cons[String, Nil.type]] = compileRegex[Cons['(', Cons['a', Cons['[', Cons['a', Cons['-', Cons['b', Cons[']', Cons['0', Cons['[', Cons['8', Cons['-', Cons['9', Cons[']', Cons[')', Nil.type]]]]]]]]]]]]]]](Cons('(', Cons('a', Cons('[', Cons('a', Cons('-', Cons('b', Cons(']', Cons('0', Cons('[', Cons('8', Cons('-', Cons('9', Cons(']', Cons(')', Nil)))))))))))))))
    val r5: String = (myPattern5("ab09"): @unchecked) match {
        case None => "none"
        case Some(Cons(s, Nil)) => s
    }

    val myPattern6: String => Option[Cons[Option[String], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons['b', Cons[')', Cons['?', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('a', Cons('b', Cons(')', Cons('?', Cons('(', Cons('c', Cons(')', Nil)))))))))
    val r6: (String, Char) = (myPattern6("abc"): @unchecked) match {
        case None => ("none", 'n')
        case Some(Cons(s, Cons(c, Nil))) => {
            if (s.isEmpty) ("none", c)
            else (s.get, c)
        }
    }

    val myPattern7: String => Option[Cons[Option[StarMatch[String]], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons['b', Cons[')', Cons['*', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('a', Cons('b', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil)))))))))
    val r7: (StarMatch[String], Char) = (myPattern7("ababc"): @unchecked) match {
        case None => (StarMatch("None"), 'n')
        case Some(Cons(s, Cons(c, Nil))) => {
            if (s.isEmpty) (StarMatch("None"), c)
            else (s.get, c)
        }
    }

    val myPattern8: String => Option[Cons[Option[StarMatch[Int]], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['1', Cons['2', Cons[')', Cons['*', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('1', Cons('2', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil)))))))))
    val r8: (StarMatch[Int], Char) = (myPattern8("12121212c"): @unchecked) match {
        case None => (StarMatch(0), 'n')
        case Some(Cons(s, Cons(c, Nil))) => {
            if (s.isEmpty) (StarMatch(0), c)
            else (s.get, c)
        }
    }

    val myPattern9: String => Option[Cons[Int, Cons[Option[StarMatch[String]], Nil.type]]] = compileRegex[Cons['(', Cons['1', Cons['2', Cons[')', Cons['(', Cons['c', Cons[')', Cons['*', Nil.type]]]]]]]]](Cons('(', Cons('1', Cons('2', Cons(')', Cons('(', Cons('c', Cons(')', Cons('*', Nil)))))))))
    val r9: (Int, StarMatch[String]) = (myPattern9("12ccc"): @unchecked) match {
        case None => (0, StarMatch("None"))
        case Some(Cons(s, Cons(c, Nil))) => {
            if (c.isEmpty) (0, StarMatch("None"))
            else (s, c.get)
        }
    }

    val myPattern10: String => Option[Cons[Option[StarMatch[String]], Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['*', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('*', Cons(')', Nil)))))))))
    val r10: StarMatch[String] = (myPattern10("abc"): @unchecked) match {
        case None => StarMatch("none")
        case Some(Cons(s, Nil)) => {
            if (s.isEmpty) StarMatch("empty")
            else s.get
        }
    }

    val myPattern11: String => Option[Cons[Option[StarMatch[Int]], Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['*', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('*', Cons(')', Nil)))))))))
    val r11: StarMatch[Int] = (myPattern11("123"): @unchecked) match {
        case None => StarMatch(0)
        case Some(Cons(s, Nil)) => {
            if (s.isEmpty) StarMatch(0)
            else s.get
        }
    }

    val myPattern12: String => Option[Cons[Int, Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['?', Cons['1', Cons[')', Nil.type]]]]]]]]]](Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('?', Cons('1', Cons(')', Nil))))))))))
    val r12: Int = (myPattern12("1"): @unchecked) match {
        case None => 0
        case Some(Cons(s, Nil)) => s
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
        assert(r9 == (12, StarMatch("c")), s"Found $r9, expected (12, StarMatch(c))")
        assert(r10 == StarMatch("abc"), s"Found $r10, expected StarMatch(abc)")
        assert(r11 == StarMatch(123), s"Found $r11, expected StarMatch(123)")
        assert(r12 == 1, s"Found $r12, expected 1")
    }
}

object RegexTests {
    import Lst._
    import Regex._

    val y1: RegexError.type = compileRegex[Cons['(', Nil.type]](Cons('(', Nil))
    val y2: RegexError.type = compileRegex[Cons['(', Cons['[', Cons[')', Nil.type]]]](Cons('(', Cons('[', Cons(')', Nil))))

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
    val x19: String => Option[Cons[Option[StarMatch[String]], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['a', Cons[')', Cons['*', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]](Cons('(', Cons('a', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil))))))))
    val x20: String => Option[Cons[Option[StarMatch[Int]], Cons[Char, Nil.type]]] = compileRegex[Cons['(', Cons['1', Cons[')', Cons['*', Cons['(', Cons['c', Cons[')', Nil.type]]]]]]]](Cons('(', Cons('1', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Nil))))))))
    val x21: String => Option[Cons[Option[StarMatch[Int]], Cons[Option[StarMatch[String]], Nil.type]]] = compileRegex[Cons['(', Cons['1', Cons[')', Cons['*', Cons['(', Cons['c', Cons[')', Cons['*', Nil.type]]]]]]]]](Cons('(', Cons('1', Cons(')', Cons('*', Cons('(', Cons('c', Cons(')', Cons('*', Nil)))))))))
    val x22: String => Option[Cons[Option[StarMatch[String]], Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['*', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('*', Cons(')', Nil)))))))))
    val x23: String => Option[Cons[Option[Char], Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['?', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('?', Cons(')', Nil)))))))))
    val x24: String => Option[Cons[Option[StarMatch[Int]], Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['*', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('*', Cons(')', Nil)))))))))
    val x25: String => Option[Cons[Option[Int], Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['?', Cons[')', Nil.type]]]]]]]]](Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('?', Cons(')', Nil)))))))))
    val x26: String => Option[Cons[Int, Nil.type]] = compileRegex[Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['?', Cons[')', Nil.type]]]]]]]]]]]]]](Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('?', Cons(')', Nil))))))))))))))
}

// object Benchmarks {
//     import Lst._
//     import CheckDelimiters._
//     import Regex._

//     // type XS2 = Cons['(', Cons[')', Nil.type]]
//     // type XS4 = Cons['(', Cons['a', Cons['z', Cons[')', Nil.type]]]]
//     // type XS8 = Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Nil.type]]]]]]]]
//     // type XS12 = Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Nil.type]]]]]]]]]]]]
//     // type XS24 = Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Nil.type]]]]]]]]]]]]]]]]]]]]]]]]
//     // type XS48 = Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Nil.type]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
//     // type XS96 = Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Nil.type]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
//     type XS156 = Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Cons['(', Cons['a', Cons['z', Cons[')', Nil.type]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

//     // val x2 = Cons('(', Cons(')', Nil))
//     // val x4 = Cons('(', Cons('a', Cons('z', Cons(')', Nil))))
//     // val x8 = Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Nil))))))))
//     // val x12 = Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Nil))))))))))))
//     // val x24 = Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Nil))))))))))))))))))))))))
//     // val x48 = Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Nil))))))))))))))))))))))))))))))))))))))))))))))))
//     // val x96 = Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
//     val x156 = Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Cons('(', Cons('a', Cons('z', Cons(')', Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
//     val x = compileRegex[XS156](x156)
// }

// object Examples {
//     import Lst._
//     import Regex._

//     case class URL(protocol: Option[String], hostname: String, domain: String, path: Option[StarMatch[String]])

//     val regexURL: String => Option[Cons[Option[String], Cons[Option[String], Cons[String, Cons[Char, Cons[String, Cons[Option[StarMatch[String]], Nil.type]]]]]]] = compileRegex[Cons['(', Cons['h', Cons['t', Cons['t', Cons['p', Cons['s', Cons['?', Cons[':', Cons['/', Cons['/', Cons[')', Cons['?', Cons['(', Cons['w', Cons['w', Cons['w', Cons['.', Cons[')', Cons['?', Cons['(', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['*', Cons[')', Cons['(', Cons['.', Cons[')', Cons['(', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['*', Cons[')', Cons['(', Cons['/', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['*', Cons[')', Cons['*', Nil.type]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]](Cons('(', Cons('h', Cons('t', Cons('t', Cons('p', Cons('s', Cons('?', Cons(':', Cons('/', Cons('/', Cons(')', Cons('?', Cons('(', Cons('w', Cons('w', Cons('w', Cons('.', Cons(')', Cons('?', Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('*', Cons(')', Cons('(', Cons('.', Cons(')', Cons('(', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('*', Cons(')', Cons('(', Cons('/', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('*', Cons(')', Cons('*', Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
//     val url: Option[URL] = regexURL("https://www.epfl.ch/schools/ic/") map {
//         case Cons(protocol, Cons(_, Cons(hostname, Cons(_, Cons(domain, Cons(path, _)))))) => URL(protocol, hostname, domain, path) // here if an optional is not present, its value is None and its type is known at compile time
//     }

//     val protocolName: String = extractProtocolName(url)

//     def extractProtocolName(url: Option[URL]): String = url match {
//         case Some(URL(prot, _, _, _)) => prot match {
//         case Some(s) => s.dropRight(3)
//         case _ => "No protocol"
//         }
//         case _ => "No URL"
//     }

//     //---------------------------------------------------------------------------------

//     val regexCompilationResult: String => Option[Cons[String, Cons[Int, Cons[String, Cons[String, Cons[Char, Cons[Int, Cons[String, Cons[Int, Cons[Char, Cons[Int, Cons[Char, Cons[Int, Cons[Char, Cons[Int, Cons[Char, Cons[String, Nil.type]]]]]]]]]]]]]]]]] = compileRegex[Cons['(', Cons['T', Cons['o', Cons['t', Cons['a', Cons['l', Cons[' ', Cons['t', Cons['i', Cons['m', Cons['e', Cons[':', Cons[' ', Cons[')', Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['*', Cons[')', Cons['(', Cons[' ', Cons['s', Cons[',', Cons[' ', Cons['c', Cons['o', Cons['m', Cons['p', Cons['l', Cons['e', Cons['t', Cons['e', Cons['d', Cons[' ', Cons[')', Cons['(', Cons['[', Cons['A', Cons['-', Cons['Z', Cons[']', Cons['[', Cons['a', Cons['-', Cons['z', Cons[']', Cons['*', Cons[')', Cons['(', Cons[' ', Cons[')', Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['?', Cons[')', Cons['(', Cons[',', Cons[' ', Cons[')', Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons[')', Cons['(', Cons[' ', Cons[')', Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['?', Cons[')', Cons['(', Cons[':', Cons[')', Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons[')', Cons['(', Cons[':', Cons[')', Cons['(', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons['[', Cons['0', Cons['-', Cons['9', Cons[']', Cons[')', Cons['(', Cons[' ', Cons[')', Cons['(', Cons['[', Cons['A', Cons['-', Cons['Z', Cons[']', Cons['[', Cons['A', Cons['-', Cons['Z', Cons[']', Cons[')', Nil.type]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]](Cons('(', Cons('T', Cons('o', Cons('t', Cons('a', Cons('l', Cons(' ', Cons('t', Cons('i', Cons('m', Cons('e', Cons(':', Cons(' ', Cons(')', Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('*', Cons(')', Cons('(', Cons(' ', Cons('s', Cons(',', Cons(' ', Cons('c', Cons('o', Cons('m', Cons('p', Cons('l', Cons('e', Cons('t', Cons('e', Cons('d', Cons(' ', Cons(')', Cons('(', Cons('[', Cons('A', Cons('-', Cons('Z', Cons(']', Cons('[', Cons('a', Cons('-', Cons('z', Cons(']', Cons('*', Cons(')', Cons('(', Cons(' ', Cons(')', Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('?', Cons(')', Cons('(', Cons(',', Cons(' ', Cons(')', Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Cons('(', Cons(' ', Cons(')', Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('?', Cons(')', Cons('(', Cons(':', Cons(')', Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Cons('(', Cons(':', Cons(')', Cons('(', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons('[', Cons('0', Cons('-', Cons('9', Cons(']', Cons(')', Cons('(', Cons(' ', Cons(')', Cons('(', Cons('[', Cons('A', Cons('-', Cons('Z', Cons(']', Cons('[', Cons('A', Cons('-', Cons('Z', Cons(']', Cons(')', Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
//     val compilationTime: Option[Int] = regexCompilationResult("Total time: 228 s, completed May 9, 2019 7:33:37 PM") map {
//         case Cons(_, Cons(time, _)) => time // completely safe
//     }
// }
