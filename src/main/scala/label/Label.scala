package label
import scala.collection.mutable.HashMap         


class Label(){
    var labelmap = new   HashMap[String,Set[String]]       // map contains owner : string , reader set for owner  : for one owner : reader set 
    def addLabel(owner : String, readerset : Set[String]) ={
      labelmap(owner) = readerset;
    }
    def removeLabel(owner : String) = {
      if(labelmap.contains(owner)){
        labelmap -= owner;
      }
    }
    def updateLabel(owner : String, readerset : Set[String]) = {
      if(labelmap.contains(owner)){
        labelmap(owner) = readerset;
      }
    }
    def getLabel : HashMap[String,Set[String]] = {
      return  labelmap
    }

}




 /*       
    
    
      * 
      * 
      * 
      * var owner : String = ""
    var reader  :  Set[String] = Set()                    // reader  set containing readers for an owner // immutable set by default
      */
//   / }