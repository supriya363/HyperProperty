package model


abstract class Expression 

case class Number(value : Int)  extends Expression {
    override def toString() = {
        value.toString
    }
}
case class ExpIdentifier( name : String) extends Expression {
    override def toString() = {
        name
    }
}
case class ExpStringLit(name : String) extends Expression
case class Plus(left: Expression, right: Expression) extends Expression {
    override def toString() = {
        left.toString + " + " + right.toString + ""
    }
}
case class Minus(left: Expression, right: Expression) extends Expression {
    override def toString() = {
        left.toString + " - " + right.toString + ""
    }
}
case class Product(left: Expression, right: Expression) extends Expression {
    override def toString() = {
        left.toString + " * " + right.toString + ""
    }
}
case class GreaterThan( left: Expression,  right: Expression) extends Expression {
    override def toString() = {
        left.toString + " > " + right.toString + ""
    }
}
case class LessThan( left: Expression,  right: Expression) extends Expression {
    override def toString() = {
        left.toString + " < " + right.toString + ""
    }
}
case class EqualsTo( left: Expression, right: Expression) extends Expression {
    override def toString() = {
        left.toString + " == " + right.toString + ""
    }
}
case class Concat ( left : Expression , right: Expression )  extends Expression {
    left.toString + right.toString
}
case class Stoi(e1 : Expression)  extends Expression 
case class Itos(e1 : Expression)  extends Expression
case class And(left: Expression, right: Expression) extends Expression {
    override def toString() = {
        "(" + left.toString + " AND " + right.toString + ")"
    }
}
case class Or(left: Expression, right: Expression) extends Expression {
    override def toString() = {
        "(" + left.toString + " OR " + right.toString + ")"
    }
}
case class Not(expr: Expression) extends Expression {
    override def toString() = {
        " ( NOT (" + expr.toString + ") )"
    }
}
//add empty expression -> skip


