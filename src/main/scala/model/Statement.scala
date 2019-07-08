package model

abstract class Statement 
case class VariableDeclaration(name: Expression, datatype : String) extends Statement {
    override def toString() = {
        "var " + name.toString + ": " + datatype + ";\n"
    }
}
case class VariableDefinition(name: Expression, value: Expression) extends Statement {
    override def toString() = {
        name.toString + " = " + value.toString + ";\n"
    }
}
case class PrintStatement(value: Expression) extends Statement
case class IfStatement(condition: Expression, trueBranch: List[Statement], falseBranch: List[Statement]) extends Statement {
    override def toString() = {
        var name : Statement = PrintStatement(ExpStringLit(""))
        var resultTrue : String = ""
        var resultFalse : String = ""
        for(name <- trueBranch)
            resultTrue += name.toString
        for(name <- falseBranch)
            resultFalse += name.toString
        if(falseBranch.isEmpty)
        {    "if " + condition.toString + " then\n" + resultTrue + "endif\n"
        }
        else
        {
            "if " + condition.toString + " then\n" + resultTrue + "else \n" + resultFalse + "endif\n"
        }
    }
}
case class WhileLoop(condition: Expression, trueBranch: List[Statement]) extends Statement {
    override def toString() = {
        var name : Statement = PrintStatement(ExpStringLit(""))
        var resultTrue : String = ""
        for(name <- trueBranch)
            resultTrue += name.toString
        "while " + condition.toString + " do\n" + resultTrue + "endwhile\n"
    }
}
// case class Skip() extends Statement

// ADDED
case class FunctionCall(name :String, parameter : List[Expression], returnParameter: List[Expression]) extends Statement {
    override def toString() = {
        // var name : Statement = PrintStatement(ExpStringLit(""))
        var parameters : String = ""
        var returnParameters : String = ""
        for(name <- parameter) {
            parameters = parameters + name.toString + ","
        }
        for(name <- returnParameter) {
            returnParameters = returnParameters + name.toString + ","
        }
        parameters = parameters.substring(0, parameters.length-1)
        if(!returnParameter.isEmpty)
        returnParameters = returnParameters.substring(0, returnParameters.length-1)
        if(returnParameter.isEmpty)
            "\n" +"call " + name + "(" + parameters + ");\n"
        else
            "\n" +"call " + name + "(" + parameters + ") returns (" + returnParameters + ");\n"
    }
}