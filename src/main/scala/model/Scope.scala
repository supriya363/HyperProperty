package model
import scala.collection.mutable.HashMap


class Scope() 
{
    var variableValue =  new HashMap[String,Datatype]         // variable name - value (string / int ) map
}