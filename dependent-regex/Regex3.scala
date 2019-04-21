object ListCharConcat {
    import Regex.StarMatch
    sealed trait List {
        def ++(that: List): List =
            if (this.isInstanceOf[Nil.type]) that
            else Cons[Char, List](this.asInstanceOf[Cons[Char, List]].head, this.asInstanceOf[Cons[Char, List]].tail ++ that) // need to be modified
        }

        type Concat[L1 <: List, L2 <: List] = L1 match {
            case Nil.type => L2
            case Cons[x, xs] => Cons[x, Concat[xs, L2]]
        }

        type Init[L1 <: List] = L1 match {
            case Cons[x, Nil.type] => Nil.type
            case Cons[x, xs] => Cons[x, Init[xs]]
        }

        type ToTypesList[GroupsTypesRepr <: List] = GroupsTypesRepr match {
            case Nil.type => Nil.type
            case Cons[x, xs] => x match {
                case 'C' => Cons[x, ToTypesList[xs]]
                case 'S' => Cons["s", ToTypesList[xs]]
                case 'H' => Cons[Some['c'], ToTypesList[xs]]
                case 'T' => Cons[Some["s"], ToTypesList[xs]]
                case 'N' => Cons[Some[0], ToTypesList[xs]]
                case 'R' => Cons[Some[StarMatch['c']], ToTypesList[xs]]
                case 'G' => Cons[Some[StarMatch["s"]], ToTypesList[xs]]
                case _ => Cons[Some[StarMatch[0]], ToTypesList[xs]]
        }
    }

        case object Nil extends List
        case class Cons[T, Tail <: List](head: T, tail: Tail) extends List
}

object Nat {
    sealed trait Nat

    class Zero extends Nat

    class Suc[P <: Nat] extends Nat

    class Pred[P <: Nat] extends Nat

    type One = Suc[Zero]
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

    // can probably remove CachedRegex in all match types
    type Compile[Input <: List, CurrType <: Type, Chars <: Nat, CharClass <: Boolean, Classes <: Nat, GroupsTypesRepr <: List, CachedRegex <: List] = CharClass match {
        case false => Input match {
            case Nil.type => ReturnType[CachedRegex, GroupsTypesRepr]
            case Cons['[', xs] => Compile[xs, CurrType, Chars, true, Suc[Classes], GroupsTypesRepr, CachedRegex]
            case Cons['(', xs] => Compile[xs, Empty.type, Zero, false, Zero, GroupsTypesRepr, CachedRegex]
            case Cons[')', xs] => Compile[xs, CurrType, Chars, CharClass, Classes, AddTypeToList[CurrType, GroupsTypesRepr, Chars], CachedRegex]
            case Cons['?', xs] => Compile[xs, CurrType, Chars, CharClass, Classes, AddTypeToList[Optional[CurrType], Init[GroupsTypesRepr], Chars], CachedRegex]
            case Cons['*', xs] => Compile[xs, CurrType, Chars, CharClass, Classes, AddTypeToList[Star[CurrType], Init[GroupsTypesRepr], Chars], CachedRegex]
            case Cons[x, xs] => IsDigit[x] match {
                case true => CurrType match {
                    case Empty.type => Compile[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]
                    case Integ.type => Compile[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]
                    case _ => Compile[xs, Str.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]
                }
                case false => Compile[xs, Str.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]
            }
        }
        case true => CheckBrackets[Input] match {
            case false => RegexError.type
            case true => CompileCharClass[Input, Empty.type, Zero, false, Zero, Nil.type, Input, ' ']
        }
    }

    type CompileCharClass[Input <: List, CurrType <: Type, Chars <: Int, CharClass <: Boolean, Classes <: Int, GroupsTypesRepr <: List, CachedRegex <: List, FirstElemInClass <: Char] = Input match {
        case Cons['-', xs] => CompileCharClass[xs, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, FirstElemInClass]
        case Cons[']', xs] => Classes match {
            case One => CurrType match {
                case Str.type => Compile[xs, Chr.type, Chars, false, Classes, GroupsTypesRepr, CachedRegex]
                case _ => Compile[xs, CurrType, Chars, false, Classes, GroupsTypesRepr, CachedRegex]
            }
            case _ => Compile[xs, CurrType, Chars, false, Classes, GroupsTypesRepr, CachedRegex]
        }
        case Cons[x, xs] => IsDigit[x] match {
            case true => CurrType match {
                case Empty.type => CompileCharClass[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, x]
                case Integ.type => CompileCharClass[xs, Integ.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, x]
                case _ => CompileCharClass[xs, Str.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, x]
            }
            case false => CompileCharClass[xs, Str.type, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex, x]
        }
    }

    type AddTypeToList[T <: Type, L <: List, Chars <: Int] = T match {
        case Optional[t] => t match {
            case Chr.type => Concat[L, Cons['H', Nil.type]]
            case Str.type => Concat[L, Cons['T', Nil.type]]
            case _ => Chars match {
                case One => Concat[L, Cons['H', Nil.type]]
                case _ => Concat[L, Cons['N', Nil.type]]
            }
        }
        case Star[t] => t match {
            case Chr.type => Concat[L, Cons['R', Nil.type]]
            case Str.type => Concat[L, Cons['G', Nil.type]]
            case _ => Chars match {
                case One => Concat[L, Cons['R', Nil.type]]
                case _ => Concat[L, Cons['E', Nil.type]]
            }
        }
        case Chr.type => Concat[L, Cons['C', Nil.type]]
        case Str.type => Concat[L, Cons['S', Nil.type]]
        case _ => Chars match {
            case One => Concat[L, Cons['C', Nil.type]]
            case _ => Concat[L, Cons['I', Nil.type]]
        }
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

    type ReturnType[CachedRegex <: String, GroupsTypesRepr <: List] = String => Option[ToTypesList[GroupsTypesRepr]]

    def compileRegex[Input <: List](s: Input): CompileRegex[Input] = {
        compile(s, Empty, 0, false, 0, Nil, s)
    }.asInstanceOf[CompileRegex[Input]]

    def compile[Input <: List, CurrType <: Type, Chars <: Nat, CharClass <: Boolean, Classes <: Nat, GroupsTypesRepr <: List, CachedRegex <: List](s: Input, currType: CurrType, chars: Int, charClass: CharClass, classes: Int, groupsTypesRepr: GroupsTypesRepr, cachedRegex: => CachedRegex): Compile[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex] = {
        // TODO
    }.asInstanceOf[Compile[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]]


    //val x1: String => Option[Cons["s", Cons['C', Nil.type]]] = compileRegex(Cons('(', Cons('a', Cons('s', Cons('d', Cons('f', Cons('s', Cons(')', Cons('(', Cons('a', Cons(')', Nil)))))))))))
}

object RegexTests {
    import ListCharConcat._
    import Regex._

    val x1: RegexError.type = compileRegex[Cons['(', Nil.type]](Cons('(', Nil))
}
