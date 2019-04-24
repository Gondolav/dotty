object ListCharConcat {
    import Regex.StarMatch
    sealed trait List {
        def toScalaList: scala.collection.immutable.List[Any] =
            if (this.isInstanceOf[Nil.type]) Nil.empty
            else this.asInstanceOf[Cons].head :: this.asInstanceOf[Cons].tail.toScalaList
    }
    case object Nil extends List
    case class Cons[T, Tail <: List](head: T, tail: Tail) extends List

    type Concat[L1 <: List, L2 <: List] = L1 match {
            case Nil.type => L2
            case Cons[x, xs] => Cons[x, Concat[xs, L2]]
    }

    type ToTypesList[L <: List] = L match {
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

    class Suc[P <: Nat] extends Nat

    class Pred[P <: Nat] extends Nat
}

object CheckParens {
    import ListCharConcat._
    import Nat._

    type CheckParens[Input <: List] = Check[Input, Zero, '(', ')']

    type CheckBrackets[Input <: List] = Check[Input, Zero, '[', ']']

    type Check[Input <: List, Opened <: Nat, Open <: Char, Close <: Char] = Input match {
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
    import ListCharConcat._
    import CheckParens._
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

    type CompileRegex[Input <: List] = CheckParens[Input] match {
        case true => Compile[Input, Empty.type, Zero, false, Zero, Nil.type, Input]
        case false => RegexError.type
    }

    type Compile[Input <: List, CurrType <: Type, Chars <: Nat, CharClass <: Boolean, Classes <: Nat, GroupsTypesRepr <: List, CachedRegex <: List] = CharClass match {
        case false => Input match {
            case Nil.type => String => Option[ToTypesList[GroupsTypesRepr]]
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

    type CompileCharClass[Input <: List, CurrType <: Type, Chars <: Nat, CharClass <: Boolean, Classes <: Nat, GroupsTypesRepr <: List, FirstElemInClass <: Char, CachedRegex] = Input match {
        case Cons['-', xs] => CompileCharClass[xs, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, FirstElemInClass, CachedRegex]
        case Cons[']', xs] => Classes match {
            case Suc[Zero] => CurrType match {
                case Str.type => Compile[xs, Chr.type, Chars, false, Classes, GroupsTypesRepr, CachedRegex]
                case _ => Compile[xs, CurrType, Chars, false, Classes, GroupsTypesRepr, CachedRegex]
            }
            case _ => Compile[xs, CurrType, Chars, false, Classes, GroupsTypesRepr, CachedRegex]
        }
        case Cons[x, xs] => IsDigit[x] match { // TODO RegexError if FirstElemInClass > x
            case true => CurrType match {
                case Empty.type => CompileCharClass[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, x, CachedRegex]
                case Integ.type => CompileCharClass[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, x, CachedRegex]
                case _ => CompileCharClass[xs, Str.type, Chars, CharClass, Classes, GroupsTypesRepr, x, CachedRegex]
            }
            case false => CompileCharClass[xs, Str.type, Chars, CharClass, Classes, GroupsTypesRepr, x, CachedRegex]
        }
    }

    type AddTypeToList[T <: Type, L <: List, Chars <: Nat] = T match {
        case Optional[t] => t match {
            case Chr.type => Concat[L, Cons['H', Nil.type]]
            case Str.type => Chars match {
                case Suc[Zero] => Concat[L, Cons['H', Nil.type]]
                case _ => Concat[L, Cons['T', Nil.type]]
            }
            case _ => Concat[L, Cons['N', Nil.type]]
        }
        case Star[t] => t match {
            case Chr.type => Concat[L, Cons['R', Nil.type]]
            case Str.type => Chars match {
                case Suc[Zero] => Concat[L, Cons['R', Nil.type]]
                case _ => Concat[L, Cons['G', Nil.type]]
            }
            case _ => Concat[L, Cons['E', Nil.type]]
        }
        case Chr.type => Concat[L, Cons['C', Nil.type]]
        case Str.type => Chars match {
                case Suc[Zero] => Concat[L, Cons['C', Nil.type]]
                case _ => Concat[L, Cons['S', Nil.type]]
            }
        case _ => Concat[L, Cons['I', Nil.type]]
    }

    type IsDigit[C <: Char] = C match {
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

    def compileRegex[Input <: List](s: Input): CompileRegex[Input] = {
        compile[Input, Empty.type, Zero, false, Zero, Nil.type, Input](s, Empty, 0, false, 0, Nil, s)
    }.asInstanceOf[CompileRegex[Input]]

    def compile[Input <: List, CurrType <: Type, Chars <: Nat, CharClass <: Boolean, Classes <: Nat, GroupsTypesRepr <: List, CachedRegex <: List](s: Input, currType: CurrType, chars: Int, charClass: CharClass, classes: Int, groupsTypesRepr: GroupsTypesRepr, cachedRegex: => CachedRegex): Compile[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex] = {
        // TODO
    }.asInstanceOf[Compile[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]]
}

object RegexTests {
    import ListCharConcat._
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
