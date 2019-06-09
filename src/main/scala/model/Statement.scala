package model

abstract class Statement 
case class VariableDeclaration(name: Expression, datatype : String) extends Statement
case class VariableDefinition(name: Expression, value: Expression) extends Statement
case class PrintStatement(value: Expression) extends Statement
case class IfStatement(condition: Expression, trueBranch: List[Statement], falseBranch: List[Statement]) extends Statement
case class WhileLoop(condition: Expression, trueBranch: List[Statement]) extends Statement