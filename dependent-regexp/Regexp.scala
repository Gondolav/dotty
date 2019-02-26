object Regexp {
    dependent def compileRegex(s: String): Any = {
        // require(checkParens(s))
        val groups = s.split(Array('(', ')')).filter(_.nonEmpty)

        if (groups(0).length == 1 && groups(0)(0).isLetter) null.asInstanceOf[String => Char]
        else if (groups(0) forall Character.isDigit) null.asInstanceOf[String => Int]
        else null.asInstanceOf[String => String]
    }

    val x1 = compileRegex("(asdfs)")
    val x2: String => Char | (Int | String) = compileRegex("(a)")
    val x3: String => Int = compileRegex("(123)")
}
