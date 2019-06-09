package model


abstract class Expression 

case class Number(value : Int)  extends Expression
case class ExpIdentifier( name : String) extends Expression
case class ExpStringLit(name : String) extends Expression
case class Plus(left: Expression, right: Expression) extends Expression
case class Minus(left: Expression, right: Expression) extends Expression
case class Product(left: Expression, right: Expression) extends Expression
case class GreaterThan( left: Expression,  right: Expression) extends Expression
case class LessThan( left: Expression,  right: Expression) extends Expression
case class EqualsTo( left: Expression, right: Expression) extends Expression
case class Concat (left :Expression , right:Expression )  extends Expression
case class Stoi(e1 : Expression)  extends Expression
case class Itos(e1 : Expression)  extends Expression

  


