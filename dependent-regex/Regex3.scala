object ListCharConcat {
    sealed trait List {
        def ++(that: List): List =
            if (this.isInstanceOf[Nil.type]) that
            else Cons[Char, List](this.asInstanceOf[Cons[Char, List]].head, this.asInstanceOf[Cons[Char, List]] ++ that)
        }

        case object Nil extends List
        case class Cons[T <: Char, Tail <: List](head: T, tail: Tail) extends List
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

    sealed trait Type
    case object Str extends Type
    case object Integ extends Type
    case object Chr extends Type
    case object Empty extends Type
    case class Optional(tp: Type) extends Type
    case class Star(tp: Type) extends Type

    case object RegexError
    case class StarMatch[T](m: T)

    type CompileRegex[Input <: List] = CheckParens[Input] match {
        case true => Compile[Input, Empty.type, 0, false, 0, Nil.type, Input]
        case false => RegexError.type
    }

    // can probably remove CachedRegex in all match types
    type Compile[Input <: List, CurrType <: Type, Chars <: Int, CharClass <: Boolean, Classes <: Int, GroupsTypesRepr <: List, CachedRegex <: List] = CharClass match {
        case false => Input match {
            case Nil.type => ReturnType[CachedRegex, GroupsTypesRepr]
            case Cons['[', xs] => Compile[xs, CurrType, Chars, true, Classes + 1, GroupsTypesRepr, CachedRegex]
            case Cons['(', xs] => Compile[xs, Empty.type, 0, false, 0, GroupsTypesRepr, CachedRegex]
            case Cons[')', xs] => Compile[xs, CurrType, Chars, CharClass, Classes, AddTypeToList[currType, groupsTypesRepr, chars], CachedRegex]
            case Cons['?', xs] => Compile[xs, CurrType, Chars, CharClass, Classes, AddTypeToList[Optional[currType], GroupsTypesRepr.asInstanceOf[Cons].init, Chars], CachedRegex]
            case Cons['*', xs] => Compile[xs, CurrType, Chars, CharClass, Classes, AddTypeToList[Star[currType], GroupsTypesRepr.asInstanceOf[Cons].init, Chars], CachedRegex]
            case Cons[x, xs] => // TODO
        }
        case true => CheckBrackets[Input] match {
            case false => RegexError.type
            case true => CompileCharClass[Input, Empty.type, 0, false, 0, Nil.type, Input, ' ']
        }
    }

    type CompileCharClass[Input <: List, CurrType <: Type, Chars <: Int, CharClass <: Boolean, Classes <: Int, GroupsTypesRepr <: List, CachedRegex <: List, FirstElemInClass <: Char] = {
        // TODO
    }

    type ReturnType[CachedRegex <: String, GroupsTypesRepr <: List] = String => Option[ToTypesList[GroupsTypesRepr]]

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

    def compileRegex[Input <: List](s: Input): CompileRegex[Input] = {

    }.asInstanceOf[CompileRegex[Input]]

    def compile[Input <: List, CurrType <: Type, Chars <: Int, CharClass <: Boolean, Classes <: Int, GroupsTypesRepr <: List, CachedRegex <: List](s: Input, currType: Type, chars: Int, charClass: Boolean, classes: Int, groupsTypesRepr: List, cachedRegex: => List): Compile[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex] = {
        // TODO
    }.asInstanceOf[Compile[Input, CurrType, Chars, CharClass, Classes, GroupsTypesRepr, CachedRegex]]

}

object RegexTests {
    import ListCharConcat._
    import Regex._

    val x1: RegexError.type = compileRegex[Cons['(', Nil.type]](Cons('(', Nil))
}
