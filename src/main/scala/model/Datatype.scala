package model

abstract class Datatype

case class int(value: Int) extends Datatype
case class string(value: String) extends Datatype
//ADDED
case class func(value : String) extends Datatype // default ret val

