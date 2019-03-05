object Regexp {
    import scala.collection.mutable.ListBuffer

    sealed abstract class Type
    case object Str extends Type {
        override def toString(): String = "String"
    }
    case object Integ extends Type {
        override def toString(): String = "Int"
    }
    case object Chr extends Type {
        override def toString(): String = "Char"
    }


    val a = compileRegex("(") // error: looping type
    a.apply("") // error: method apply not found on type Error

    trait Error
    dependent def compileRegex(s: String): Any = {
        if (!checkParens(s)) new Error

        // require(checkParens(s), "The groups are not well defined")
        val groups = findGroups(s)

        //val groupsTypes: List[Type] = Nil

        //for (group <- groups.reverse) {
            if (groups(0).length == 1 && groups(0)(0).isLetter)
              runtimeCompileCharRegex(s).asInstanceOf[String => Char]
            else if (groups(0).forall(c => c.isDigit)) return null.asInstanceOf[String => Int]
            else if (groups(0).contains('[') || groups(0).contains(']')) return handleCharacterClasses(groups(0))
            else return null.asInstanceOf[String => String]
        //}


    }

    def runtimeCompileCharRegex(s: String): String => Any =
        input => s.r.firstMatchIn(input).map(ma => (ma(1).toChar))

    dependent def compileRegex2(s: String): Any = {
        // require(checkParens(s), "The groups are not well defined")
        val groups = findGroups(s)

        var groupsTypes: ListBuffer[Type] = ListBuffer()

        for (group <- groups) {
            if (group.length == 1 && group(0).isLetter) groupsTypes += Chr
            else if (group.forall(c => c.isDigit)) groupsTypes += Integ
            else if (group.contains('[') || group.contains(']')) handleCharacterClasses2(group, groupsTypes)
            else groupsTypes += Str
        }

        groupsTypes.toList.foreach(println)
    }

    private def findGroups(s: String): Array[String] = s.split(Array('(', ')')).filter(_.nonEmpty)

    private def findClasses(s: String): Array[String] = s.split(Array('[', ']')).filter(_.nonEmpty)

    private def handleCharacterClasses(s: String): Any = {
        //require(checkBrackets(s), "The character class is not well defined")
        val classes = findClasses(s)

        if (classes.length == 1 && classes(0).split('-').forall(s => s forall(c => c.isDigit))) return null.asInstanceOf[String => Char]

        var t: Type = Str

        for (c <- classes) {
            if (c.split('-').forall(s => s forall Character.isDigit)) t = Integ
        }

        (t: @unchecked) match {
            case Str => return null.asInstanceOf[String => String]
            case Integ => return null.asInstanceOf[String => Int]
        }
    }

    private def handleCharacterClasses2(s: String, groupsTypes: ListBuffer[Type]): Unit = {
        //require(checkBrackets(s), "The character class is not well defined")
        val classes = findClasses(s)

        if (classes.length == 1 && classes(0).split('-').forall(s => s forall(c => c.isDigit))) groupsTypes += Chr

        var t: Type = Str

        for (c <- classes) {
            if (c.split('-').forall(s => s forall Character.isDigit)) t = Integ
        }

        (t: @unchecked) match {
            case Str => groupsTypes += Str
            case Integ => groupsTypes += Integ
        }
    }

    val x = compileRegex2("(asdfs)(23)")

    val x1: String => Char = compileRegex("(asdfs)")
    val x2: String => Char = compileRegex("(a)")
    val x3: String => Int = compileRegex("(123)")
    val x4: String => String = compileRegex("[a-z][a-z]")
    val x5: String => Int = compileRegex("[0-9]")
    val x6: String => Char = compileRegex("[A-Z]")

    (x2("a"): Option[Char]) == Some('a')
    (x2("b"): Option[Char]) == None

    e1("b("): // error
    e2("b("): // error

    (x7("b"): Option[(Int, Int, String)]) == None

    doesItMatches(regex: String, input: String): Boolean
}
