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
    sealed trait Nat {
        type N <: Nat
    }

    class Zero extends Nat {
        type N = Zero
    }

    class Suc[P <: Nat] extends Nat {
        type N = P match {
            case Pred[Suc[P]] => P
            case _ => Suc[P]
        }
    }

    class Pred[P <: Nat] extends Nat {
        type N = Pred[P]
    }

}

object CheckParens {
    import ListCharConcat._
    import Nat._

    type CheckParens[Input <: List] = Input match {
        case _ => Check[Input, Zero, '(', ')']
    }

    type CheckBrackets[Input <: List] = Input match {
        case _ => Check[Input, Zero, '[', ']']
    }

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

}
