def checkParens(s: List[Char]): Boolean = {
    def check(cs: List[Char], opened: Int): Boolean = cs match {
        case Nil => opened == 0
        case '(' :: xs => check(xs, opened + 1)
        case ')' :: _ if opened < 1 => false
        case ')' :: xs => check(xs, opened - 1)
        case _ :: xs => check(xs, opened)
    }
    check(s, 0)
}
