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
        case _ => CheckParens0[Input, Zero]
    }

    type CheckParens0[Input <: List, Opened <: Nat] = Input match {
        case Nil.type => Opened match {
            case Zero => true
            case _ => false
        }
        case Cons['(', xs] => Opened match {
            case Pred[o] => CheckParens0[xs, o]
            case _ => CheckParens0[xs, Suc[Opened]]
        }
        case Cons[')', xs] => Opened match {
                case Zero => false
                case _ => Opened match {
                    case Suc[o] => CheckParens0[xs, o]
                    case _ => CheckParens0[xs, Pred[Opened]]
                }
            }
        case Cons[_, xs] => CheckParens0[xs, Opened]
    }

        val balancedEmpty: CheckParens[Nil.type] = true
        val balanced1: CheckParens[Cons['(', Cons['(', Cons[')', Cons[')', Nil.type]]]]] = true
        val balanced2: CheckParens[Cons['a', Cons['(', Cons['s', Cons['(', Cons['v', Cons['v', Cons[')', Cons['d', Cons[')', Cons['f', Cons['f', Nil.type]]]]]]]]]]]] = true
        val balanced3: CheckParens[Cons['(', Cons[')', Cons['(', Cons[')', Nil.type]]]]] = true

        val notBalanced1: CheckParens[Cons['(', Cons['(', Cons[')', Nil.type]]]] = false
        val notBalanced2: CheckParens[Cons['(', Cons['(', Cons[')', Cons[')', Cons[')', Cons['(', Nil.type]]]]]]] = false

        // val balancedBrackets1: true = checkBrackets(Cons('[', Cons('[', Cons(']', Cons(']', Nil)))))
}
