object Regexp {

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

    dependent def compileRegex(s: String): Any = {
        // require(checkParens(s), "The groups are not well defined")
        val groups = findGroups(s)

        val groupsTypes: List[Type] = Nil

        //for (group <- groups.reverse) {
            if (groups(0).length == 1 && groups(0)(0).isLetter) return null.asInstanceOf[String => Char]
            else if (groups(0).forall(c => c.isDigit)) return null.asInstanceOf[String => Int]
            else if (groups(0).contains('[') || groups(0).contains(']')) return handleCharacterClasses(groups(0))
            else return null.asInstanceOf[String => String]
        //}


    }

    dependent def compileRegex2(s: String): Any = {
        // require(checkParens(s), "The groups are not well defined")
        val groups = findGroups(s)

        var groupsTypes: Array[Type] = Array.empty

        for (group <- groups) {
            if (group.length == 1 && group(0).isLetter) groupsTypes = groupsTypes :+ Chr
            else if (group.forall(c => c.isDigit)) groupsTypes = groupsTypes :+ Integ
            else if (group.contains('[') || group.contains(']')) groupsTypes = handleCharacterClasses2(group, groupsTypes)
            else groupsTypes = groupsTypes :+ Str
        }

        groupsTypes.foreach(println)
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

    private def handleCharacterClasses2(s: String, groupsTypes: Array[Type]): Array[Type] = {
        //require(checkBrackets(s), "The character class is not well defined")
        val classes = findClasses(s)

        if (classes.length == 1 && classes(0).split('-').forall(s => s forall(c => c.isDigit))) return groupsTypes :+ Chr

        var t: Type = Str

        for (c <- classes) {
            if (c.split('-').forall(s => s forall Character.isDigit)) t = Integ
        }

        (t: @unchecked) match {
            case Str => return groupsTypes :+ Str
            case Integ => return groupsTypes :+ Integ
        }
    }

    val x = compileRegex2("(asdfs)(23)")

    val x1: String => String = compileRegex("(asdfs)")
    val x2: String => Char = compileRegex("(a)")
    val x3: String => Int = compileRegex("(123)")
    val x4: String => String = compileRegex("[a-z][a-z]")
    val x5: String => Int = compileRegex("[0-9]")
    val x6: String => Char = compileRegex("[A-Z]")
}
