package typecheck
import label._
import scala.collection.mutable.HashMap

class ParamTable(){
  var paramSymTable =  new HashMap[String,ParamInfo]                   //  symbol table stores varname - type
  def isOfSameType(symbolname : String, othersymbolname : String) : Boolean ={
     if(paramSymTable.get(symbolname).get.datatype.equals(paramSymTable.get(othersymbolname).get.datatype) )
       return true;
     else
       return false;
  }
}


